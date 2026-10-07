;;; agent-shell-quota.el --- Claude and Codex account quota headers -*- lexical-binding: t; -*-

;;; Commentary:
;; Show remaining account quota in Claude and Codex agent-shell headers.
;; Shared quota-only connections reuse each provider's login.  Read on session
;; open, input submission, and turn completion; never poll while idle or send
;; inference prompts.  Claude uses the SDK bundled with its ACP adapter.
;; Quota colors use the same faces and used-percentage thresholds as context
;; usage.  Reset times appear in brackets and in the header's hover details.
;; M-x sdb/agent-shell-quota-refresh requests an immediate refresh.
;; Run bin/test-agent-shell-quota.sh for the isolated ERT suite.

;;; Code:

(require 'agent-shell)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'parse-time)

(defvar sdb/agent-shell-quota--process nil "Shared quota-only Codex connection.")
(defvar sdb/agent-shell-quota--timer nil "Obsolete polling timer, cancelled on reload.")
(defvar sdb/agent-shell-quota--data nil "Last validated quota windows.")
(defvar sdb/agent-shell-quota--updated-at nil "Timestamp of the last quota snapshot.")
(defvar sdb/agent-shell-quota--last-error nil "Transport failure shown in quota tooltips.")
(defvar sdb/agent-shell-quota--previous-provider nil "Header provider restored on disable.")
(defvar sdb/agent-shell-quota--claude-process nil "Shared quota-only Claude connection.")
(defvar sdb/agent-shell-quota--claude-data nil "Last validated Claude quota windows.")
(defvar sdb/agent-shell-quota--claude-updated-at nil "Last Claude snapshot timestamp.")
(defvar sdb/agent-shell-quota--claude-last-error nil "Claude quota transport failure.")
(defvar-local sdb/agent-shell-quota--subscription nil "Shell activity subscription token.")
(defconst sdb/agent-shell-quota--claude-helper
  (expand-file-name "../bin/claude-quota.mjs"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Quota helper shipped with this extension.")
(defvar sdb/agent-shell-quota--timeout-seconds 30 "Maximum wait for an RPC response.")
(defvar sdb/agent-shell-quota-mode nil "Non-nil when quota headers are enabled.")

(defcustom sdb/agent-shell-quota-show-reset-time t
  "Whether quota items include their reset time in brackets.
For example, \"5h: 64% left [16:28]\" or \"7d: 85% left [Sunday 15:47]\".
Times use the local time zone.  Full timestamps remain in hover details."
  :type 'boolean
  :group 'agent-shell)

(defun sdb/agent-shell-quota--provider (state)
  "Return the quota provider for STATE, or nil for unsupported agents."
  (pcase (map-nested-elt state '(:agent-config :identifier))
    ('codex 'codex)
    ('claude-code 'claude)))

(defun sdb/agent-shell-quota--buffers (&optional provider)
  "Return existing PROVIDER shells; default to Codex, or use `all'."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'agent-shell-mode)
            (let ((agent (sdb/agent-shell-quota--provider agent-shell--state)))
              (if (eq provider 'all) agent (eq agent (or provider 'codex)))))))
   (buffer-list)))

(defun sdb/agent-shell-quota--redraw (&rest _)
  "Refresh quota-bearing headers, including viewport headers."
  (dolist (buffer (sdb/agent-shell-quota--buffers 'all))
    (with-current-buffer buffer
      (agent-shell--update-header-and-mode-line)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* (((derived-mode-p 'agent-shell-viewport-view-mode
                                   'agent-shell-viewport-edit-mode))
                  (shell (agent-shell-viewport--shell-buffer))
                  ((sdb/agent-shell-quota--provider
                    (buffer-local-value 'agent-shell--state shell))))
        (agent-shell-viewport--update-header)))))

(defun sdb/agent-shell-quota--window (snapshot minutes)
  "Return a valid quota window of MINUTES from SNAPSHOT, or nil."
  (seq-find
   (lambda (window)
     (let ((used (map-elt window 'usedPercent)))
       (and (equal minutes (map-elt window 'windowDurationMins))
            (numberp used) (<= 0 used 100))))
   (list (map-elt snapshot 'primary) (map-elt snapshot 'secondary))))

(defun sdb/agent-shell-quota--save (result)
  "Save Codex quota RESULT without mixing different metered buckets."
  (let* ((fallback (map-elt result 'rateLimits))
         (snapshot (or (map-nested-elt result '(rateLimitsByLimitId codex))
                       (when (member (map-elt fallback 'limitId) '(nil "codex"))
                         fallback))))
    (setq sdb/agent-shell-quota--data
          (list (cons :five-hour (sdb/agent-shell-quota--window snapshot 300))
                (cons :seven-day (sdb/agent-shell-quota--window snapshot 10080)))
          sdb/agent-shell-quota--updated-at (float-time)
          sdb/agent-shell-quota--last-error nil)
    (sdb/agent-shell-quota--redraw)))

(defun sdb/agent-shell-quota--reset-label (reset &optional weekday)
  "Return RESET's local bracketed time, adding a date if it is not today.
With WEEKDAY non-nil, always show the weekday instead of a date.
For example, return \"[16:28]\" or \"[Sunday 15:47]\"; return nil if unknown."
  (when (and (numberp reset) (> reset 0))
    (let ((time (seconds-to-time reset)))
      (format-time-string
       (cond (weekday "[%A %H:%M]")
             ((equal (format-time-string "%Y-%m-%d" time)
                     (format-time-string "%Y-%m-%d" (current-time)))
              "[%H:%M]")
             (t "[%b %d %H:%M]"))
       time))))

(defun sdb/agent-shell-quota--indicator (key label &optional provider)
  "Format quota KEY as LABEL, for example \"5h: 64% left [16:28]\".
Stale percentages have a tilde prefix and the theme's shadow face."
  (let* ((claude (eq provider 'claude))
         (data (if claude sdb/agent-shell-quota--claude-data sdb/agent-shell-quota--data))
         (updated-at (if claude sdb/agent-shell-quota--claude-updated-at sdb/agent-shell-quota--updated-at))
         (last-error (if claude sdb/agent-shell-quota--claude-last-error sdb/agent-shell-quota--last-error))
         (window (map-elt data key))
         (used (map-elt window 'usedPercent))
         (remaining (when (numberp used) (round (- 100 used))))
         (reset (map-elt window 'resetsAt))
         (stale (and window
                     (or last-error
                         (not updated-at)
                         (and (numberp reset) (<= reset (float-time))))))
         (face (if (or (null remaining) stale)
                   'shadow
                 (agent-shell--context-usage-face used)))
         (help (concat
                (format "%s account quota (%s), shared across sessions.\n"
                        (if claude "Claude" "Codex") label)
                (if remaining
                    (format "%s%% left; resets %s."
                            remaining
                            (if (numberp reset)
                                (format-time-string "%Y-%m-%d %H:%M %Z" (seconds-to-time reset))
                              "at an unknown time"))
                  "Quota unavailable.")
                (when updated-at
                  (format "\nLast refreshed %s."
                          (format-time-string "%H:%M:%S %Z"
                                              (seconds-to-time updated-at))))
                (when stale "\nStale snapshot; awaiting a successful refresh.")
                (when last-error (concat "\n" last-error)))))
    (propertize (if remaining
                    (concat (format "%s: %s%d%% left" label (if stale "~" "") remaining)
                            (when-let* ((sdb/agent-shell-quota-show-reset-time)
                                        (reset-label (sdb/agent-shell-quota--reset-label
                                                      reset (eq key :seven-day))))
                              (concat " " reset-label)))
                  (format "%s: ?" label))
                'face face 'font-lock-face face 'help-echo help)))

(defun sdb/agent-shell-quota--header-indicators (state)
  "Return themed quota strings for STATE, preserving other header providers."
  (append (when sdb/agent-shell-quota--previous-provider
            (funcall sdb/agent-shell-quota--previous-provider state))
          (when-let* ((provider (sdb/agent-shell-quota--provider state)))
            (list (sdb/agent-shell-quota--indicator :five-hour "5h" provider)
                  (sdb/agent-shell-quota--indicator :seven-day "7d" provider)))))

(defun sdb/agent-shell-quota--stop (&optional provider)
  "Stop the shared quota process and cancel its request timeout."
  (when-let* ((process (if (eq provider 'claude)
                           sdb/agent-shell-quota--claude-process
                         sdb/agent-shell-quota--process)))
    (if (eq provider 'claude)
        (setq sdb/agent-shell-quota--claude-process nil)
      (setq sdb/agent-shell-quota--process nil))
    (when-let* ((timeout (process-get process :timeout)))
      (cancel-timer timeout))
    (when (process-live-p process)
      (delete-process process))
    (when-let* ((stderr (process-get process :stderr))
                ((buffer-live-p stderr)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer stderr)))))

(defun sdb/agent-shell-quota--fail (reason &optional provider)
  "Retain the last snapshot, mark it stale with REASON, and stop transport."
  (if (eq provider 'claude)
      (setq sdb/agent-shell-quota--claude-last-error reason)
    (setq sdb/agent-shell-quota--last-error reason))
  (sdb/agent-shell-quota--stop provider)
  (sdb/agent-shell-quota--redraw))

(defun sdb/agent-shell-quota--send (process message)
  "Send one JSON MESSAGE to PROCESS, without logging credentials or responses."
  (process-send-string process (concat (json-serialize message) "\n")))

(defun sdb/agent-shell-quota--current-process-p (process)
  "Return non-nil if PROCESS owns its provider's current connection."
  (eq process (if (eq (process-get process :provider) 'claude)
                  sdb/agent-shell-quota--claude-process
                sdb/agent-shell-quota--process)))

(defun sdb/agent-shell-quota--timeout (process id)
  "Fail request ID only if PROCESS still owns the pending request."
  (when (and (sdb/agent-shell-quota--current-process-p process)
             (equal id (process-get process :pending-id)))
    (sdb/agent-shell-quota--fail
     "Quota request timed out; retry on the next interaction."
     (process-get process :provider))))

(defun sdb/agent-shell-quota--request (process method &optional params)
  "Send METHOD with PARAMS to PROCESS, allowing only one outstanding request."
  (unless (process-get process :pending-id)
    (let ((id (1+ (or (process-get process :sequence) 0))))
      (process-put process :sequence id)
      (process-put process :pending-id id)
      (process-put process :pending-method method)
      (process-put process :timeout
                   (run-at-time sdb/agent-shell-quota--timeout-seconds nil
                                #'sdb/agent-shell-quota--timeout process id))
      (sdb/agent-shell-quota--send
       process (append (list (cons 'id id) (cons 'method method))
                       (when params (list (cons 'params params))))))))

(defun sdb/agent-shell-quota--receive (process message)
  "Handle a parsed app-server MESSAGE from the current PROCESS."
  (let ((id (map-elt message 'id))
        (method (map-elt message 'method)))
    (cond
     ((and id method)
      (sdb/agent-shell-quota--send
       process `((id . ,id) (error . ((code . -32601) (message . "Unsupported request"))))))
     ((equal method "account/rateLimits/updated")
      (let ((params (map-elt message 'params)))
        (when (or (map-nested-elt params '(rateLimitsByLimitId codex))
                  (member (map-nested-elt params '(rateLimits limitId)) '(nil "codex")))
          (sdb/agent-shell-quota--save params))))
     ((and id (equal id (process-get process :pending-id)))
      (let ((pending-method (process-get process :pending-method)))
        (when-let* ((timeout (process-get process :timeout)))
          (cancel-timer timeout))
        (process-put process :timeout nil)
        (process-put process :pending-id nil)
        (process-put process :pending-method nil)
        (cond
         ((map-elt message 'error)
          (sdb/agent-shell-quota--fail "Quota request failed; check your provider login and SDK version."
                                       (process-get process :provider)))
         ((equal pending-method "initialize")
          (process-put process :ready t)
          (sdb/agent-shell-quota--send process '((method . "initialized")))
          (sdb/agent-shell-quota--request process "account/rateLimits/read"))
         ((equal pending-method "account/rateLimits/read")
          (sdb/agent-shell-quota--save (map-elt message 'result)))
         ((equal pending-method "usage/read")
          (sdb/agent-shell-quota--save-claude (map-elt message 'result))))
        (when (and (sdb/agent-shell-quota--current-process-p process)
                   (process-get process :refresh-again)
                   (not (equal pending-method "initialize")))
          (process-put process :refresh-again nil)
          (sdb/agent-shell-quota--request
           process (if (eq (process-get process :provider) 'claude)
                       "usage/read" "account/rateLimits/read"))))))))

(defun sdb/agent-shell-quota--filter (process output)
  "Parse newline-delimited OUTPUT from PROCESS, retaining incomplete lines."
  (when (sdb/agent-shell-quota--current-process-p process)
    (condition-case nil
        (let ((pending (concat (process-get process :partial) output))
              newline)
          (while (and (sdb/agent-shell-quota--current-process-p process)
                      (setq newline (string-match "\n" pending)))
            (let ((line (substring pending 0 newline)))
              (setq pending (substring pending (1+ newline)))
              (unless (string-empty-p (string-trim line))
                (sdb/agent-shell-quota--receive
                 process (json-parse-string line :object-type 'alist :array-type 'list
                                            :null-object nil :false-object :false)))))
          (if (> (length pending) (* 1024 1024))
              (sdb/agent-shell-quota--fail "Invalid quota response; retry on the next interaction."
                                           (process-get process :provider))
            (process-put process :partial pending)))
      (error (sdb/agent-shell-quota--fail "Could not read quota; retry on the next interaction."
                                          (process-get process :provider))))))

(defun sdb/agent-shell-quota--sentinel (process _event)
  "Handle an unexpected exit of the active quota PROCESS."
  (when (and (sdb/agent-shell-quota--current-process-p process)
             (not (process-live-p process)))
    (sdb/agent-shell-quota--fail "Quota connection closed; retry on the next interaction."
                                 (process-get process :provider))))

(defun sdb/agent-shell-quota--start ()
  "Start the quota-only app server using the existing local Codex login."
  (let ((executable (executable-find "codex")))
    (unless executable (error "Codex executable unavailable"))
    (let* ((default-directory user-emacs-directory)
           (stderr (generate-new-buffer " *Codex quota stderr*"))
           (process
            (condition-case error-data
                (make-process :name "sdb-codex-quota" :buffer nil :stderr stderr
                              :command (list executable "app-server" "--listen" "stdio://")
                              :coding 'utf-8-unix :connection-type 'pipe :noquery t
                              :filter #'sdb/agent-shell-quota--filter
                              :sentinel #'sdb/agent-shell-quota--sentinel)
              (error (kill-buffer stderr) (signal (car error-data) (cdr error-data))))))
      (setq sdb/agent-shell-quota--process process)
      (process-put process :provider 'codex)
      (process-put process :stderr stderr)
      (sdb/agent-shell-quota--request
       process "initialize"
       '((clientInfo . ((name . "sdb_emacs_quota")
			(title . "Emacs quota indicator") (version . "1"))))))))

(defun sdb/agent-shell-quota--claude-window (window)
  "Normalize a Claude WINDOW without inventing missing percentages or resets."
  (let ((used (map-elt window 'utilization))
        (reset (map-elt window 'resets_at)))
    (when (and (numberp used) (<= 0 used 100))
      `((usedPercent . ,used)
        (resetsAt . ,(when (stringp reset)
                       (ignore-errors (float-time (date-to-time reset)))))))))

(defun sdb/agent-shell-quota--save-claude (result)
  "Save Claude account quota RESULT independently of Codex."
  (setq sdb/agent-shell-quota--claude-data
        (list (cons :five-hour (sdb/agent-shell-quota--claude-window (map-elt result 'five_hour)))
              (cons :seven-day (sdb/agent-shell-quota--claude-window (map-elt result 'seven_day))))
        sdb/agent-shell-quota--claude-updated-at (float-time)
        sdb/agent-shell-quota--claude-last-error nil)
  (sdb/agent-shell-quota--redraw))

(defun sdb/agent-shell-quota--start-claude ()
  "Start a quota-only reader using the SDK bundled with Claude's ACP adapter."
  (let ((node (executable-find "node"))
        (acp (executable-find (car agent-shell-anthropic-claude-acp-command)))
        (claude (executable-find "claude")))
    (unless (and node acp claude) (error "Claude quota executables unavailable"))
    (let* ((default-directory temporary-file-directory)
           (stderr (generate-new-buffer " *Claude quota stderr*"))
           (process
            (condition-case error-data
                (make-process :name "sdb-claude-quota" :buffer nil :stderr stderr
                              :command (list node sdb/agent-shell-quota--claude-helper acp claude)
                              :coding 'utf-8-unix :connection-type 'pipe :noquery t
                              :filter #'sdb/agent-shell-quota--filter
                              :sentinel #'sdb/agent-shell-quota--sentinel)
              (error (kill-buffer stderr) (signal (car error-data) (cdr error-data))))))
      (setq sdb/agent-shell-quota--claude-process process)
      (process-put process :provider 'claude)
      (process-put process :stderr stderr)
      (process-put process :ready t)
      (sdb/agent-shell-quota--request process "usage/read"))))

(defun sdb/agent-shell-quota--refresh-provider (provider)
  "Refresh PROVIDER asynchronously, combining overlapping activity requests."
  (condition-case nil
      (if (not (sdb/agent-shell-quota--buffers provider))
          (sdb/agent-shell-quota--stop provider)
        (let ((process (if (eq provider 'claude)
                           sdb/agent-shell-quota--claude-process
                         sdb/agent-shell-quota--process)))
          (if (process-live-p process)
              (cond
               ((process-get process :pending-id)
                ;; One follow-up read captures a turn that finished during a read.
                (process-put process :refresh-again t))
               ((process-get process :ready)
                (sdb/agent-shell-quota--request
                 process (if (eq provider 'claude) "usage/read" "account/rateLimits/read"))))
            (sdb/agent-shell-quota--stop provider)
            (if (eq provider 'claude)
                (sdb/agent-shell-quota--start-claude)
              (sdb/agent-shell-quota--start))))
        (sdb/agent-shell-quota--redraw))
    (error (sdb/agent-shell-quota--fail
            "Quota unavailable; check your provider executable and login." provider))))

(defun sdb/agent-shell-quota-refresh (&optional provider)
  "Refresh PROVIDER's account quota without sending an agent prompt.
Interactively refresh both Claude and Codex.  No idle polling is scheduled."
  (interactive)
  (when sdb/agent-shell-quota-mode
    (dolist (agent (if provider (list provider) '(codex claude)))
      (sdb/agent-shell-quota--refresh-provider agent))))

(defun sdb/agent-shell-quota--on-event (event)
  "Refresh the current shell's quota for relevant activity EVENTs."
  (when-let* ((sdb/agent-shell-quota-mode)
              (provider (sdb/agent-shell-quota--provider agent-shell--state)))
    (pcase (map-elt event :event)
      ((or 'init-finished 'input-submitted 'turn-complete)
       (sdb/agent-shell-quota-refresh provider))
      ('clean-up
       (unless (seq-remove (lambda (buffer) (eq buffer (current-buffer)))
                           (sdb/agent-shell-quota--buffers provider))
         (sdb/agent-shell-quota--stop provider))))))

(defun sdb/agent-shell-quota--attach ()
  "Subscribe to activity in the current supported shell exactly once."
  (when (and sdb/agent-shell-quota-mode
             (derived-mode-p 'agent-shell-mode)
             (sdb/agent-shell-quota--provider agent-shell--state)
             (not sdb/agent-shell-quota--subscription))
    (setq sdb/agent-shell-quota--subscription
          (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                    :on-event #'sdb/agent-shell-quota--on-event))
    (sdb/agent-shell-quota-refresh
     (sdb/agent-shell-quota--provider agent-shell--state))))

(defun sdb/agent-shell-quota--stop-all ()
  "Stop both quota transports."
  (sdb/agent-shell-quota--stop)
  (sdb/agent-shell-quota--stop 'claude))

(define-minor-mode sdb/agent-shell-quota-mode
  "Display themed 5-hour and 7-day account quota in Claude and Codex headers."
  :global t
  :group 'agent-shell
  ;; Cancel the old polling timer when upgrading a running Emacs.
  (when sdb/agent-shell-quota--timer
    (cancel-timer sdb/agent-shell-quota--timer)
    (setq sdb/agent-shell-quota--timer nil))
  (remove-hook 'kill-emacs-hook #'sdb/agent-shell-quota--stop)
  (if sdb/agent-shell-quota-mode
      (progn
        (unless (eq agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators)
          (setq sdb/agent-shell-quota--previous-provider agent-shell-header-extra-indicators-function
                agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators))
        (add-hook 'agent-shell-mode-hook #'sdb/agent-shell-quota--attach)
        (dolist (buffer (sdb/agent-shell-quota--buffers 'all))
          (with-current-buffer buffer (sdb/agent-shell-quota--attach)))
        (add-hook 'enable-theme-functions #'sdb/agent-shell-quota--redraw)
        (add-hook 'disable-theme-functions #'sdb/agent-shell-quota--redraw)
        (add-hook 'kill-emacs-hook #'sdb/agent-shell-quota--stop-all))
    (remove-hook 'agent-shell-mode-hook #'sdb/agent-shell-quota--attach)
    (dolist (buffer (sdb/agent-shell-quota--buffers 'all))
      (with-current-buffer buffer
        (when sdb/agent-shell-quota--subscription
          (agent-shell-unsubscribe :subscription sdb/agent-shell-quota--subscription)
          (setq sdb/agent-shell-quota--subscription nil))))
    (sdb/agent-shell-quota--stop-all)
    (when (eq agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators)
      (setq agent-shell-header-extra-indicators-function sdb/agent-shell-quota--previous-provider))
    (remove-hook 'enable-theme-functions #'sdb/agent-shell-quota--redraw)
    (remove-hook 'disable-theme-functions #'sdb/agent-shell-quota--redraw)
    (remove-hook 'kill-emacs-hook #'sdb/agent-shell-quota--stop-all))
  (sdb/agent-shell-quota--redraw))

(provide 'agent-shell-quota)
;;; agent-shell-quota.el ends here
