;;; agent-shell-quota-tests.el --- Quota tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Isolated tests use a pipe process and fake RPC responses, never a Codex turn.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-quota)

(defmacro sdb/agent-shell-quota-tests--isolated (&rest body)
  "Run BODY without touching the user's quota process or timers."
  (declare (indent 0) (debug t))
  `(let ((sdb/agent-shell-quota--process nil)
         (sdb/agent-shell-quota--timer nil)
         (sdb/agent-shell-quota--data nil)
         (sdb/agent-shell-quota--claude-process nil)
         (sdb/agent-shell-quota--claude-data nil)
         (sdb/agent-shell-quota--claude-updated-at nil)
         (sdb/agent-shell-quota--claude-last-error nil)
         (agent-shell-mode-hook (copy-sequence agent-shell-mode-hook))
         (sdb/agent-shell-quota--updated-at nil)
         (sdb/agent-shell-quota--last-error nil)
         (sdb/agent-shell-quota--previous-provider nil)
         (sdb/agent-shell-quota-show-reset-time nil)
         (sdb/agent-shell-quota-mode nil)
         (agent-shell-header-extra-indicators-function nil)
         (enable-theme-functions (copy-sequence enable-theme-functions))
         (disable-theme-functions (copy-sequence disable-theme-functions))
         (kill-emacs-hook (copy-sequence kill-emacs-hook)))
     (cl-letf (((symbol-function 'sdb/agent-shell-quota--redraw) #'ignore))
	      (unwind-protect (progn ,@body)
		(sdb/agent-shell-quota-mode -1)))))

(defmacro sdb/agent-shell-quota-tests--with-process (&rest body)
  "Run BODY with a local pipe PROCESS and captured outgoing MESSAGES."
  (declare (indent 0) (debug t))
  `(let ((process (make-pipe-process :name "quota-test" :buffer nil :noquery t))
         messages)
     (setq sdb/agent-shell-quota--process process)
     (cl-letf (((symbol-function 'process-send-string)
                (lambda (_process string)
                  (push (json-parse-string string :object-type 'alist) messages))))
	      (unwind-protect (progn ,@body)
		(sdb/agent-shell-quota--stop)))))

(defun sdb/agent-shell-quota-tests--result (five seven)
  "Make a Codex quota response with FIVE and SEVEN percent used."
  (copy-tree
   `((rateLimits . ((limitId . "codex")
                    (primary . ((usedPercent . ,five) (windowDurationMins . 300)
				(resetsAt . 2000000000)))
                    (secondary . ((usedPercent . ,seven) (windowDurationMins . 10080)
                                  (resetsAt . 2000000100))))))))

(ert-deftest sdb/agent-shell-quota-format-colors-and-reset ()
  "Percentages, severity boundaries, and reset tooltips use theme faces."
  (sdb/agent-shell-quota-tests--isolated
   (dolist (case '((0 "100% left" agent-shell-success) (59 "41% left" agent-shell-success)
                   (60 "40% left" agent-shell-warning) (84 "16% left" agent-shell-warning)
                   (85 "15% left" agent-shell-error) (100 "0% left" agent-shell-error)))
     (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result (car case) 25))
     (let ((text (sdb/agent-shell-quota--indicator :five-hour "5h")))
       (should (equal (concat "5h: " (cadr case)) text))
       (should (eq (caddr case) (get-text-property 0 'face text)))
       (should (string-match-p "resets 20" (get-text-property 0 'help-echo text)))))))

(ert-deftest sdb/agent-shell-quota-buckets-and-window-durations ()
  "Choose the Codex bucket and identify windows by duration, not position."
  (sdb/agent-shell-quota-tests--isolated
   (let* ((codex (map-elt (sdb/agent-shell-quota-tests--result 36 15) 'rateLimits))
          (first (map-elt codex 'primary)))
     (map-put! codex 'primary (map-elt codex 'secondary))
     (map-put! codex 'secondary first)
     (sdb/agent-shell-quota--save
      `((rateLimits . ((limitId . "other")))
        (rateLimitsByLimitId . ((codex . ,codex)))))
     (should (equal "5h: 64% left" (sdb/agent-shell-quota--indicator :five-hour "5h")))
     (should (equal "7d: 85% left" (sdb/agent-shell-quota--indicator :seven-day "7d"))))))

(ert-deftest sdb/agent-shell-quota-missing-and-invalid-windows ()
  "Unavailable and malformed quota values never become fabricated percentages."
  (sdb/agent-shell-quota-tests--isolated
   (dolist (result (list nil '((rateLimits . ((limitId . "other"))))
                         (sdb/agent-shell-quota-tests--result -1 "bad")
                         (sdb/agent-shell-quota-tests--result 101 nil)))
     (sdb/agent-shell-quota--save result)
     (let ((text (sdb/agent-shell-quota--indicator :five-hour "5h")))
       (should (equal "5h: ?" text))
       (should (eq 'shadow (get-text-property 0 'face text)))))))

(ert-deftest sdb/agent-shell-quota-stale-on-error-and-reset ()
  "Failed and elapsed-reset snapshots are explicitly dimmed as stale."
  (sdb/agent-shell-quota-tests--isolated
    (dolist (cause '(error reset))
     (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 36 15))
     (pcase cause
       ('error (setq sdb/agent-shell-quota--last-error "Connection closed"))
       ('reset (map-put! (map-elt sdb/agent-shell-quota--data :five-hour)
                         'resetsAt (1- (float-time)))))
     (let ((text (sdb/agent-shell-quota--indicator :five-hour "5h")))
       (should (equal "5h: ~64% left" text))
       (should (eq 'shadow (get-text-property 0 'face text)))
       (should (string-match-p "Stale snapshot" (get-text-property 0 'help-echo text)))))))

(ert-deftest sdb/agent-shell-quota-idle-snapshots-retain-percentage-colors ()
  "Idle time alone never greys a successful activity-triggered quota read."
  (sdb/agent-shell-quota-tests--isolated
    (dolist (case '((3 agent-shell-success) (60 agent-shell-warning) (85 agent-shell-error)))
      (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result (car case) 18))
      (sdb/agent-shell-quota--save-claude
       `((five_hour . ((utilization . ,(car case)) (resets_at . "2033-05-18T03:33:20Z")))))
      (setq sdb/agent-shell-quota--updated-at (- (float-time) 3600)
            sdb/agent-shell-quota--claude-updated-at sdb/agent-shell-quota--updated-at)
      (dolist (provider '(codex claude))
        (let ((text (sdb/agent-shell-quota--indicator :five-hour "5h" provider)))
          (should (eq (cadr case) (get-text-property 0 'face text)))
          (should-not (string-match-p "~" text))
          (should-not (string-match-p "Stale snapshot" (get-text-property 0 'help-echo text)))
          (should (string-match-p "Last refreshed" (get-text-property 0 'help-echo text))))))))

(ert-deftest sdb/agent-shell-quota-provider-scoped-and-composed ()
  "Other agents retain their headers, and existing providers are preserved."
  (sdb/agent-shell-quota-tests--isolated
   (setq sdb/agent-shell-quota--previous-provider (lambda (_) '("Existing")))
   (should (equal '("Existing")
                  (sdb/agent-shell-quota--header-indicators '((:agent-config . ((:identifier . claude)))))))
   (should (= 3 (length (sdb/agent-shell-quota--header-indicators
                         '((:agent-config . ((:identifier . codex))))))))))

(ert-deftest sdb/agent-shell-quota-initialize-and-read-handshake ()
  "Initialization sends only a quota read, never a thread or a turn request."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (sdb/agent-shell-quota--request process "initialize" '((clientInfo . ((name . "test")))))
    (sdb/agent-shell-quota--filter process "{\"id\":1,\"result\":{}}\n")
    (should (process-get process :ready))
    (should (equal '("initialize" "initialized" "account/rateLimits/read")
                   (mapcar (lambda (message) (map-elt message 'method)) (reverse messages))))
    (sdb/agent-shell-quota--filter
     process (concat (json-serialize `((id . 2) (result . ,(sdb/agent-shell-quota-tests--result 36 15)))) "\n"))
    (should-not (process-get process :pending-id))
    (should-not (process-get process :timeout))
    (should (equal "5h: 64% left" (sdb/agent-shell-quota--indicator :five-hour "5h"))))))

(ert-deftest sdb/agent-shell-quota-partial-and-multiple-lines ()
  "Partial NDJSON lines are retained and multiple notifications are parsed."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (let ((line (json-serialize `((method . "account/rateLimits/updated")
                                  (params . ,(sdb/agent-shell-quota-tests--result 50 25))))))
      (sdb/agent-shell-quota--filter process (substring line 0 20))
      (should-not sdb/agent-shell-quota--data)
      (sdb/agent-shell-quota--filter process (concat (substring line 20) "\r\n{\"method\":\"ignored\"}\n"))
      (should (equal "5h: 50% left" (sdb/agent-shell-quota--indicator :five-hour "5h")))
      (should (equal "" (process-get process :partial)))))))

(ert-deftest sdb/agent-shell-quota-refresh-no-overlapping-requests ()
  "Repeated refreshes reuse one initialized process and pending request."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (let ((sdb/agent-shell-quota-mode t))
      (process-put process :ready t)
      (cl-letf (((symbol-function 'sdb/agent-shell-quota--buffers) (lambda (&optional provider) (when (memq provider '(nil codex)) '(present)))))
        (sdb/agent-shell-quota-refresh)
        (sdb/agent-shell-quota-refresh))
      (should (= 1 (length messages)))
      (should (equal "account/rateLimits/read" (map-elt (car messages) 'method)))))))

(ert-deftest sdb/agent-shell-quota-unrelated-bucket-notification-ignored ()
  "Updates for another metered bucket cannot erase the Codex account quota."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 36 15))
    (sdb/agent-shell-quota--filter
     process "{\"method\":\"account/rateLimits/updated\",\"params\":{\"rateLimits\":{\"limitId\":\"other\"}}}\n")
    (should (equal "5h: 64% left" (sdb/agent-shell-quota--indicator :five-hour "5h"))))))

(ert-deftest sdb/agent-shell-quota-failed-rpc-retains-data ()
  "RPC errors retain the last snapshot, stop the process, and dim the quota."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 36 15))
    (sdb/agent-shell-quota--request process "account/rateLimits/read")
    (sdb/agent-shell-quota--filter process "{\"id\":1,\"error\":{\"code\":-1}}\n")
    (should-not sdb/agent-shell-quota--process)
    (should (equal "5h: ~64% left" (sdb/agent-shell-quota--indicator :five-hour "5h"))))))

(ert-deftest sdb/agent-shell-quota-timeout-stale-request-ignored ()
  "An old timeout cannot cancel a newer request; the current timeout can."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (sdb/agent-shell-quota--request process "account/rateLimits/read")
    (sdb/agent-shell-quota--timeout process 99)
    (should (eq process sdb/agent-shell-quota--process))
    (sdb/agent-shell-quota--timeout process 1)
    (should-not sdb/agent-shell-quota--process)
    (should (string-match-p "timed out" sdb/agent-shell-quota--last-error)))))

(ert-deftest sdb/agent-shell-quota-malformed-response-and-old-process ()
  "Invalid transport fails safely; output from a stopped connection is ignored."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (sdb/agent-shell-quota--filter process "invalid json\n")
    (should-not sdb/agent-shell-quota--process)
    (let ((reason sdb/agent-shell-quota--last-error))
      (sdb/agent-shell-quota--filter process "{\"method\":\"account/rateLimits/updated\"}\n")
      (should (equal reason sdb/agent-shell-quota--last-error))
      (should-not sdb/agent-shell-quota--data)))))

(ert-deftest sdb/agent-shell-quota-retry-and-inactive-cleanup ()
  "Failure retries on the next refresh; no Codex shells means no connection."
  (sdb/agent-shell-quota-tests--isolated
   (let ((sdb/agent-shell-quota-mode t)
         (starts 0))
     (cl-letf (((symbol-function 'sdb/agent-shell-quota--buffers) (lambda (&optional provider) (when (memq provider '(nil codex)) '(present))))
               ((symbol-function 'sdb/agent-shell-quota--start)
                (lambda () (setq starts (1+ starts)) (error "Unavailable"))))
       (sdb/agent-shell-quota-refresh)
       (sdb/agent-shell-quota-refresh))
     (should (= 2 starts))
     (sdb/agent-shell-quota-tests--with-process
      (cl-letf (((symbol-function 'sdb/agent-shell-quota--buffers) (lambda (&optional _provider) nil)))
        (sdb/agent-shell-quota-refresh))
      (should-not sdb/agent-shell-quota--process)))))

(ert-deftest sdb/agent-shell-quota-mode-idempotent-and-disable ()
  "Enabling twice preserves the provider; disabling restores it and cancels timers."
  (sdb/agent-shell-quota-tests--isolated
   (let ((provider (lambda (_) '("Existing"))))
     (setq agent-shell-header-extra-indicators-function provider)
     (sdb/agent-shell-quota-mode 1)
     (let ((timer sdb/agent-shell-quota--timer))
       (sdb/agent-shell-quota-mode 1)
       (should (eq timer sdb/agent-shell-quota--timer)))
     (should (eq provider sdb/agent-shell-quota--previous-provider))
     (should (memq #'sdb/agent-shell-quota--redraw enable-theme-functions))
     (sdb/agent-shell-quota-mode -1)
     (should (eq provider agent-shell-header-extra-indicators-function))
     (should-not sdb/agent-shell-quota--timer)
     (should-not (memq #'sdb/agent-shell-quota--redraw enable-theme-functions)))))

(ert-deftest sdb/agent-shell-quota-start-command-and-login ()
  "The background command starts app-server without login or inference requests."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (let (options)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "/test/codex"))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (setq options args) process)))
        (sdb/agent-shell-quota--start))
      (should (equal '("/test/codex" "app-server" "--listen" "stdio://")
                     (plist-get options :command)))
      (should (plist-get options :noquery))
      (should (eq 'sdb/agent-shell-quota--filter (plist-get options :filter)))
      (should (equal '("initialize") (mapcar (lambda (message) (map-elt message 'method)) messages)))
      (should-not (map-nested-elt (car messages) '(params accessToken)))))))

(ert-deftest sdb/agent-shell-quota-process-exit-marks-stale ()
  "Unexpected process exit retains data and marks the header stale."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 36 15))
    (delete-process process)
    (sdb/agent-shell-quota--sentinel process "closed")
    (should-not sdb/agent-shell-quota--process)
    (should (equal "5h: ~64% left" (sdb/agent-shell-quota--indicator :five-hour "5h"))))))

(ert-deftest sdb/agent-shell-quota-native-text-header ()
  "Native text rendering retains the extra indicators' faces and tooltips."
  (sdb/agent-shell-quota-tests--isolated
   (let* ((agent-shell-header-style 'text)
          (indicator (propertize "5h: 64% left" 'face 'success 'help-echo "Reset at noon"))
          (header (agent-shell--render-header-model-uncached
                   `((:buffer-name . "Codex")
                     (:project-name . "project") (:extra-indicators . (,indicator)))))
          (start (string-match "5h:" header)))
     (should start)
     (should (string-match-p "5h: 64%% left" header))
     (should (eq 'success (get-text-property start 'face header)))
     (should (equal "Reset at noon" (get-text-property start 'help-echo header))))))

(ert-deftest sdb/agent-shell-quota-native-graphical-header ()
  "Native SVG headers draw two quotas independently in their theme colors."
  (skip-unless (image-type-available-p 'svg))
  (sdb/agent-shell-quota-tests--isolated
   (with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (let* ((agent-shell--state '((:agent-config . ((:buffer-name . "Codex") (:identifier . codex)))
                                  (:session . ((:id . "test")))))
            (agent-shell-header-style 'graphical)
            (agent-shell--header-cache nil)
            (agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators))
       (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 10 95))
       (let* ((header (agent-shell--make-header agent-shell--state))
              (svg (plist-get (cdr (get-text-property 1 'display header)) :data)))
         (should (string-match-p (format "<tspan[^>]*fill=\"%s\"[^>]*>5h: 90%% left</tspan>"
                                         (agent-shell--svg-fill-color 'agent-shell-success)) svg))
         (should (string-match-p (format "<tspan[^>]*fill=\"%s\"[^>]*>7d: 5%% left</tspan>"
                                         (agent-shell--svg-fill-color 'agent-shell-error)) svg))
         (should (string-match-p "resets" (get-text-property 1 'help-echo header))))))))

(ert-deftest sdb/agent-shell-quota-header-cache-invalidated-by-new-data ()
  "Fresh quota data changes the native render cache key."
  (sdb/agent-shell-quota-tests--isolated
   (with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (let* ((agent-shell--state '((:agent-config . ((:buffer-name . "Codex") (:identifier . codex)))))
            (agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators))
       (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 10 15))
       (let ((first (agent-shell--header-cache-key (agent-shell--make-header-model agent-shell--state))))
         (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 20 15))
         (should-not (equal first (agent-shell--header-cache-key
                                   (agent-shell--make-header-model agent-shell--state)))))))))

(ert-deftest sdb/agent-shell-quota-reset-label-today-and-other-day ()
  "Same-day resets show local time; other days include their local date."
  (let* ((system-time-locale "C")
         (today (encode-time 0 0 12 6 10 2026))
         (same-day (float-time (encode-time 0 28 16 6 10 2026)))
         (other-day (float-time (encode-time 0 47 15 13 10 2026))))
    (cl-letf (((symbol-function 'current-time) (lambda () today)))
      (should (equal "[16:28]" (sdb/agent-shell-quota--reset-label same-day)))
      (should (equal "[Oct 13 15:47]" (sdb/agent-shell-quota--reset-label other-day)))
      (should-not (sdb/agent-shell-quota--reset-label nil))
      (should-not (sdb/agent-shell-quota--reset-label 0)))))

(ert-deftest sdb/agent-shell-quota-reset-brackets-and-hover ()
  "Reset brackets preserve percentage colors and full hover timestamps."
  (sdb/agent-shell-quota-tests--isolated
   (let ((sdb/agent-shell-quota-show-reset-time t))
     (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 75 90))
     (let ((text (sdb/agent-shell-quota--indicator :five-hour "5h")))
       (should (string-prefix-p "5h: 25% left [" text))
       (should (string-suffix-p "]" text))
       (should (eq 'agent-shell-warning (get-text-property 0 'face text)))
       (should (string-match-p "resets 2033-" (get-text-property 0 'help-echo text))))
     (map-put! (map-elt sdb/agent-shell-quota--data :five-hour) 'resetsAt nil)
     (should (equal "5h: 25% left" (sdb/agent-shell-quota--indicator :five-hour "5h"))))))

(ert-deftest sdb/agent-shell-quota-weekly-reset-always-shows-weekday ()
  "Weekly resets include the weekday even when resetting later today."
  (sdb/agent-shell-quota-tests--isolated
   (let* ((system-time-locale "C")
          (sdb/agent-shell-quota-show-reset-time t)
          (today (encode-time 0 0 12 11 10 2026))
          (reset (float-time (encode-time 0 28 16 11 10 2026))))
     (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 36 15))
     (map-put! (map-elt sdb/agent-shell-quota--data :seven-day) 'resetsAt reset)
     (cl-letf (((symbol-function 'current-time) (lambda () today)))
       (should (equal "[Sunday 16:28]" (sdb/agent-shell-quota--reset-label reset t)))
       (should (string-suffix-p " [Sunday 16:28]"
                                (sdb/agent-shell-quota--indicator :seven-day "7d")))
       (should (equal "[16:28]" (sdb/agent-shell-quota--reset-label reset)))
       (should-not (sdb/agent-shell-quota--reset-label nil t))))))

(ert-deftest sdb/agent-shell-quota-native-graphical-reset-brackets ()
  "Graphical quota items include bracketed resets and the matching context color."
  (skip-unless (image-type-available-p 'svg))
  (sdb/agent-shell-quota-tests--isolated
   (with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (let* ((sdb/agent-shell-quota-show-reset-time t)
            (agent-shell--state '((:agent-config . ((:buffer-name . "Codex") (:identifier . codex)))))
            (agent-shell-header-style 'graphical)
            (agent-shell--header-cache nil)
            (agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators))
       (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 60 85))
       (let* ((header (agent-shell--make-header agent-shell--state))
              (svg (plist-get (cdr (get-text-property 1 'display header)) :data)))
         (should (string-match-p
                  (format "<tspan[^>]*fill=\"%s\"[^>]*>5h: 40%% left \\[.*\\]</tspan>"
                          (agent-shell--svg-fill-color 'agent-shell-warning)) svg))
         (should (string-match-p
                  (format "<tspan[^>]*fill=\"%s\"[^>]*>7d: 15%% left \\[.*\\]</tspan>"
                          (agent-shell--svg-fill-color 'agent-shell-error)) svg))
         (should (string-match-p "resets 2033-" (get-text-property 1 'help-echo header))))))))

(ert-deftest sdb/agent-shell-quota-claude-windows-and-provider-isolation ()
  "Claude percentages and ISO resets render without changing Codex data."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota--save (sdb/agent-shell-quota-tests--result 36 15))
   (sdb/agent-shell-quota--save-claude
    '((five_hour . ((utilization . 3) (resets_at . "2033-05-18T03:33:20+00:00")))
      (seven_day . ((utilization . 85) (resets_at . "2033-05-18T03:35:00Z")))))
   (should (equal "5h: 97% left" (sdb/agent-shell-quota--indicator :five-hour "5h" 'claude)))
   (should (equal "5h: 64% left" (sdb/agent-shell-quota--indicator :five-hour "5h")))
   (should (= 2000000000 (map-nested-elt sdb/agent-shell-quota--claude-data '(:five-hour resetsAt))))
   (let ((text (sdb/agent-shell-quota--indicator :seven-day "7d" 'claude)))
     (should (equal "7d: 15% left" text))
     (should (eq 'agent-shell-error (get-text-property 0 'face text)))
     (should (string-prefix-p "Claude account quota" (get-text-property 0 'help-echo text))))
   (should (= 2 (length (sdb/agent-shell-quota--header-indicators
                         '((:agent-config . ((:identifier . claude-code))))))))))

(ert-deftest sdb/agent-shell-quota-claude-missing-and-malformed ()
  "Missing windows and malformed resets never fabricate Claude quota."
  (sdb/agent-shell-quota-tests--isolated
   (dolist (used '(nil "bad" -1 101))
     (sdb/agent-shell-quota--save-claude `((five_hour . ((utilization . ,used)))))
     (should (equal "5h: ?" (sdb/agent-shell-quota--indicator :five-hour "5h" 'claude))))
   (sdb/agent-shell-quota--save-claude
    '((seven_day . ((utilization . 18.5) (resets_at . "invalid")))))
   (should-not (map-nested-elt sdb/agent-shell-quota--claude-data '(:seven-day resetsAt)))
   (should (equal "7d: 82% left" (sdb/agent-shell-quota--indicator :seven-day "7d" 'claude)))))

(ert-deftest sdb/agent-shell-quota-claude-transport-and-error-isolation ()
  "Claude response failures retain its data and leave the Codex transport live."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (let ((claude (make-pipe-process :name "claude-quota-test" :buffer nil :noquery t)))
      (setq sdb/agent-shell-quota--claude-process claude)
      (process-put claude :provider 'claude)
      (sdb/agent-shell-quota--request claude "usage/read")
      (sdb/agent-shell-quota--filter claude "{\"id\":1,\"result\":{\"five_hour\":{\"utilization\":3}}}\n")
      (should (equal "5h: 97% left" (sdb/agent-shell-quota--indicator :five-hour "5h" 'claude)))
      (sdb/agent-shell-quota--request claude "usage/read")
      (sdb/agent-shell-quota--filter claude "{\"id\":2,\"error\":{\"message\":\"PRIVATE SDK DETAIL\"}}\n")
      (should-not sdb/agent-shell-quota--claude-process)
      (should (eq process sdb/agent-shell-quota--process))
      (should-not sdb/agent-shell-quota--last-error)
      (let ((text (sdb/agent-shell-quota--indicator :five-hour "5h" 'claude)))
        (should (equal "5h: ~97% left" text))
        (should-not (string-match-p "PRIVATE" (get-text-property 0 'help-echo text))))))))

(ert-deftest sdb/agent-shell-quota-overlapping-refresh-keeps-one-follow-up ()
  "A completion during a pending read gets one follow-up, never parallel reads."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (let ((sdb/agent-shell-quota-mode t))
      (process-put process :ready t)
      (cl-letf (((symbol-function 'sdb/agent-shell-quota--buffers)
                 (lambda (&optional _) '(present))))
        (dotimes (_ 3) (sdb/agent-shell-quota-refresh 'codex)))
      (should (= 1 (length messages)))
      (sdb/agent-shell-quota--filter process "{\"id\":1,\"result\":{}}\n")
      (should (= 2 (length messages)))
      (should (equal 2 (process-get process :pending-id)))
      (sdb/agent-shell-quota--filter process "{\"id\":2,\"result\":{}}\n")
      (should-not (process-get process :pending-id))
      (should (= 2 (length messages)))))))

(ert-deftest sdb/agent-shell-quota-activity-subscriptions-no-polling ()
  "Existing and new shells subscribe once and refresh only on relevant activity."
  (sdb/agent-shell-quota-tests--isolated
   (with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (setq-local agent-shell--state
                 (agent-shell--make-state
                  :buffer (current-buffer) :agent-config '((:identifier . claude-code))))
     (let (refreshes)
       (cl-letf (((symbol-function 'sdb/agent-shell-quota--refresh-provider)
                  (lambda (provider) (push provider refreshes))))
         (sdb/agent-shell-quota-mode 1)
         (sdb/agent-shell-quota-mode 1)
         (should (equal '(claude) refreshes))
         (should-not sdb/agent-shell-quota--timer)
         (should (= 1 (length (map-elt agent-shell--state :event-subscriptions))))
         (dolist (event '(init-finished input-submitted turn-complete idle agent-message-chunk))
           (agent-shell--emit-event :event event))
         (should (equal '(claude claude claude claude) refreshes))
         (sdb/agent-shell-quota-mode -1)
         (should-not sdb/agent-shell-quota--subscription)
         (should-not (map-elt agent-shell--state :event-subscriptions))
         (should-not (memq #'sdb/agent-shell-quota--attach agent-shell-mode-hook))
         ;; Simulate a newly opened shell through the mode hook.
         (setq sdb/agent-shell-quota-mode t)
         (add-hook 'agent-shell-mode-hook #'sdb/agent-shell-quota--attach)
         (run-hooks 'agent-shell-mode-hook)
         (should sdb/agent-shell-quota--subscription)
         (agent-shell--emit-event :event 'turn-complete)
         (should (= 6 (length refreshes)))
         (sdb/agent-shell-quota-mode -1))))))

(ert-deftest sdb/agent-shell-quota-upgrade-cancels-old-timer ()
  "Upgrading removes the prior recurring timer without scheduling another."
  (sdb/agent-shell-quota-tests--isolated
   (let ((timer (run-at-time 3600 60 #'ignore)))
     (setq sdb/agent-shell-quota--timer timer)
     (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) (ert-fail "Unexpected timer"))))
       (sdb/agent-shell-quota-mode 1))
     (should-not sdb/agent-shell-quota--timer)
     (should-not (memq timer timer-list)))))

(ert-deftest sdb/agent-shell-quota-last-shell-cleanup ()
  "Closing a provider's last shell closes its quota connection."
  (sdb/agent-shell-quota-tests--isolated
   (sdb/agent-shell-quota-tests--with-process
    (with-temp-buffer
      (setq major-mode 'agent-shell-mode)
      (setq-local agent-shell--state '((:agent-config . ((:identifier . codex)))))
      (let ((sdb/agent-shell-quota-mode t))
        (sdb/agent-shell-quota--on-event '((:event . clean-up)))
        (should-not sdb/agent-shell-quota--process))))))

(ert-deftest sdb/agent-shell-quota-claude-native-graphical-header ()
  "Claude uses the same native header colors and reset labels as Codex."
  (skip-unless (image-type-available-p 'svg))
  (sdb/agent-shell-quota-tests--isolated
   (with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (let* ((agent-shell--state '((:agent-config . ((:buffer-name . "Claude") (:identifier . claude-code)))))
            (sdb/agent-shell-quota-show-reset-time t)
            (agent-shell-header-style 'graphical)
            (agent-shell-header-extra-indicators-function #'sdb/agent-shell-quota--header-indicators))
       (sdb/agent-shell-quota--save-claude
        '((five_hour . ((utilization . 60) (resets_at . "2033-05-18T03:33:20Z")))
          (seven_day . ((utilization . 85) (resets_at . "2033-05-18T03:35:00Z")))))
       (let* ((header (agent-shell--make-header agent-shell--state))
              (svg (plist-get (cdr (get-text-property 1 'display header)) :data)))
         (should (string-match-p "5h: 40% left \\[" svg))
         (should (string-match-p "7d: 15% left \\[" svg))
         (should (string-match-p "Claude account quota" (get-text-property 1 'help-echo header))))))))

(ert-deftest sdb/agent-shell-quota-viewport-redraw-resolves-owning-shell ()
  "Viewport refresh reads quota state from its shell, not the viewport mode."
  (let ((redraw (symbol-function 'sdb/agent-shell-quota--redraw)))
    (sdb/agent-shell-quota-tests--isolated
     (let* ((shell (generate-new-buffer "quota-viewport-test"))
            (viewport (generate-new-buffer
                       (concat (buffer-name shell) agent-shell-viewport--suffix)))
            (shell-updates 0)
            (viewport-updates 0))
       (unwind-protect
           (progn
             (with-current-buffer shell
               (setq major-mode 'agent-shell-mode)
               (setq-local agent-shell--state '((:agent-config . ((:identifier . claude-code))))))
             (with-current-buffer viewport
               (setq major-mode 'agent-shell-viewport-view-mode))
             (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line)
                        (lambda () (cl-incf shell-updates)))
                       ((symbol-function 'agent-shell-viewport--update-header)
                        (lambda () (cl-incf viewport-updates))))
               (funcall redraw))
             (should (= 1 shell-updates))
             (should (= 1 viewport-updates)))
         (kill-buffer viewport)
         (kill-buffer shell))))))

(provide 'agent-shell-quota-tests)
;;; agent-shell-quota-tests.el ends here
