;;; gptel-sessions.el --- Session management for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatic session saving and management for gptel with project integration
;; Supports topic-based organization for focused contexts

;;; Code:

(require 'gptel)
(require 'gptel-tools)

;;; Configuration

(defvar gptel-session-directory ".gptel"
  "Directory name for storing gptel sessions relative to project root.")

(defvar gptel-session-current-topic nil
  "Current topic/subtopic for gptel sessions.
Buffer-local variable set when starting or resuming a session.")

(make-variable-buffer-local 'gptel-session-current-topic)

;;; Helper Functions

(defun gptel-session--get-dir ()
  "Get or create .gptel directory in project root."
  (let* ((project-root (gptel-tool--ensure-git))
         (session-dir (expand-file-name gptel-session-directory project-root)))
    (unless (file-exists-p session-dir)
      (make-directory session-dir t))
    session-dir))

(defun gptel-session--get-sessions-dir (&optional topic)
  "Get sessions directory, optionally for TOPIC."
  (let* ((base-dir (gptel-session--get-dir))
         (sessions-base (expand-file-name "sessions" base-dir)))
    (unless (file-exists-p sessions-base)
      (make-directory sessions-base t))
    (if (and topic (> (length topic) 0))
        (let ((topic-dir (expand-file-name topic sessions-base)))
          (unless (file-exists-p topic-dir)
            (make-directory topic-dir t))
          topic-dir)
      sessions-base)))

(defun gptel-session--generate-name ()
  "Generate a session filename based on timestamp."
  (format "session-%s.org" (format-time-string "%Y%m%d-%H%M%S")))

(defun gptel-session--list-topics ()
  "List all available topics."
  (let* ((sessions-base (gptel-session--get-sessions-dir))
         (subdirs (directory-files sessions-base nil "^[^.]")))
    (seq-filter (lambda (d)
                  (file-directory-p (expand-file-name d sessions-base)))
                subdirs)))

(defun gptel-session--prompt-for-topic (&optional allow-new)
  "Prompt user to select a topic.
If ALLOW-NEW is non-nil, allow creating a new topic."
  (let* ((topics (gptel-session--list-topics))
         (prompt (if allow-new
                    "Topic (empty for general, new name to create): "
                  "Select topic: "))
         (topic (if topics
                   (completing-read prompt
                                   (if allow-new
                                       (append '("") topics)
                                     topics)
                                   nil
                                   (not allow-new))
                 (if allow-new
                     (read-string "Topic name (empty for general): ")
                   nil))))
    (when (and topic (string= topic ""))
      (setq topic nil))
    topic))

;;; Interactive Commands

(defun gptel-session-start (&optional topic)
  "Start a gptel session in project's .gptel directory.
With prefix arg or when TOPIC is provided, start a topic-specific session.
TOPIC can be nil for general sessions."
  (interactive
   (list (when current-prefix-arg
           (gptel-session--prompt-for-topic t))))

  (let* ((sessions-dir (gptel-session--get-sessions-dir topic))
         (session-name (gptel-session--generate-name))
         (session-file (expand-file-name session-name sessions-dir))
         (project-root (gptel-tool--ensure-git))
         (project-name (or (and (projectile-project-root)
                               (not (string= (projectile-project-name) "-"))
                               (projectile-project-name))
                          (file-name-nondirectory
                           (directory-file-name project-root))
                          "default")))

    (message "Starting gptel session: %s%s (project: %s)"
             session-name
             (if topic (format " [%s]" topic) "")
             project-name)

    ;; Create or switch to session buffer
    (with-current-buffer (find-file session-file)
      ;; Set as org-mode and enable gptel-mode
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless gptel-mode
        (gptel-mode 1))

      ;; Set buffer-local variables
      (setq-local default-directory project-root)
      (setq-local gptel-session-current-topic topic)

      ;; Add header if new file
      (when (= (buffer-size) 0)
        (insert (format "#+TITLE: GPtel Session - %s%s\n"
                       project-name
                       (if topic (format " [%s]" topic) "")))
        (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "#+PROPERTY: PROJECT_ROOT %s\n" project-root))
        (when topic
          (insert (format "#+PROPERTY: TOPIC %s\n" topic)))
        (insert "\n")
        (insert (alist-get 'org-mode gptel-prompt-prefix-alist)))

      (goto-char (point-max))
      (message "GPtel session: %s%s (working dir: %s)"
               session-name
               (if topic (format " [topic: %s]" topic) "")
               project-root)
      (current-buffer))))

(defun gptel-session-resume (&optional topic)
  "Resume an existing gptel session.
With prefix arg or when TOPIC is provided, resume from that topic."
  (interactive
   (list (when current-prefix-arg
           (gptel-session--prompt-for-topic nil))))

  (let* ((sessions-dir (gptel-session--get-sessions-dir topic))
         (sessions (directory-files sessions-dir nil "^session-.*\\.org$"))
         (project-root (or (projectile-project-root) default-directory)))
    (if sessions
        (let* ((session (completing-read
                        (format "Resume session%s: "
                               (if topic (format " [%s]" topic) ""))
                        sessions))
               (session-file (expand-file-name session sessions-dir)))
          (with-current-buffer (find-file session-file)
            (unless gptel-mode (gptel-mode 1))
            ;; Restore variables from properties or defaults
            (setq-local default-directory
                       (or (org-entry-get nil "PROJECT_ROOT" t)
                           project-root))
            (setq-local gptel-session-current-topic
                       (or (org-entry-get nil "TOPIC" t)
                           topic))
            (goto-char (point-max))
            (current-buffer)))
      (message "No sessions found in %s" sessions-dir)
      (when (y-or-n-p (format "Start a new session%s? "
                             (if topic (format " for topic '%s'" topic) "")))
        (gptel-session-start topic)))))

(defun gptel-session-list (&optional topic)
  "List all gptel sessions, optionally filtered by TOPIC."
  (interactive
   (list (when current-prefix-arg
           (gptel-session--prompt-for-topic nil))))

  (if topic
      ;; List sessions for specific topic
      (let* ((sessions-dir (gptel-session--get-sessions-dir topic))
             (sessions (directory-files sessions-dir nil "^session-.*\\.org$")))
        (if sessions
            (message "Sessions for topic '%s':\n%s"
                    topic
                    (mapconcat #'identity sessions "\n"))
          (message "No sessions found for topic '%s'" topic)))
    ;; List all topics and general sessions
    (let* ((topics (gptel-session--list-topics))
           (general-sessions (directory-files
                             (gptel-session--get-sessions-dir)
                             nil "^session-.*\\.org$")))
      (message "Topics: %s\nGeneral sessions: %d"
              (if topics (mapconcat #'identity topics ", ") "none")
              (length general-sessions)))))

(defun gptel-session-switch-topic ()
  "Switch current session to a different topic.
Creates a new session file in the target topic directory."
  (interactive)
  (unless (and (buffer-file-name)
              (string-match-p "/\\.gptel/sessions/" (buffer-file-name)))
    (user-error "Not in a gptel session buffer"))

  (let* ((new-topic (gptel-session--prompt-for-topic t))
         (current-content (buffer-string)))
    (when (y-or-n-p (format "Create new session in topic '%s' with current content? "
                           (or new-topic "general")))
      (let* ((new-sessions-dir (gptel-session--get-sessions-dir new-topic))
             (new-session-name (gptel-session--generate-name))
             (new-session-file (expand-file-name new-session-name new-sessions-dir)))
        ;; Create new session with current content
        (with-temp-file new-session-file
          (insert current-content)
          ;; Update topic property
          (goto-char (point-min))
          (if (re-search-forward "^#\\+PROPERTY: TOPIC" nil t)
              (progn
                (beginning-of-line)
                (kill-line)
                (when new-topic
                  (insert (format "#+PROPERTY: TOPIC %s\n" new-topic))))
            (when (re-search-forward "^#\\+PROPERTY: PROJECT_ROOT" nil t)
              (end-of-line)
              (insert (format "\n#+PROPERTY: TOPIC %s" (or new-topic ""))))))

        ;; Switch to new session
        (find-file new-session-file)
        (setq-local gptel-session-current-topic new-topic)
        (message "Switched to new session in topic: %s" (or new-topic "general"))))))

(defun gptel ()
  "Start or resume gptel session with project context.
With prefix ARG, behave as normal gptel (no session file)."
  (interactive)
  (if current-prefix-arg
      ;; Original behavior - just open a gptel buffer
      (let ((backend-name (format "*%s*" (gptel-backend-name gptel-backend))))
        (pop-to-buffer
         (get-buffer-create
          (read-buffer "Create or choose gptel buffer: " backend-name nil
                       (lambda (b)
                         (and-let* ((buf (get-buffer (or (car-safe b) b))))
                           (buffer-local-value 'gptel-mode buf)))))))
    ;; Session-aware behavior
    (if (and (buffer-file-name)
             (string-match-p "/\\.gptel/sessions/" (buffer-file-name)))
        ;; Already in a session, just ensure gptel-mode
        (unless gptel-mode (gptel-mode 1))
      ;; Check for existing sessions and prompt to resume
      (let* ((topics (gptel-session--list-topics))
             (general-sessions (directory-files
                               (gptel-session--get-sessions-dir)
                               nil "^session-.*\\.org$"))
             (has-sessions (or general-sessions topics)))
        (if (and has-sessions
                 (y-or-n-p "Resume existing session? "))
            (if topics
                ;; Prompt for topic first
                (let ((choice (completing-read
                              "Select: "
                              (append '("General") topics))))
                  (if (string= choice "General")
                      (gptel-session-resume nil)
                    (gptel-session-resume choice)))
              (gptel-session-resume nil))
          ;; Start new session
          (gptel-session-start))))))

;;; Auto-save Hook

(defun gptel-session--auto-save (start end)
  "Auto-save gptel session buffer after each response.
START and END are the response boundaries (unused but required by hook)."
  (when (and (buffer-file-name)
             (string-match-p "/\\.gptel/" (buffer-file-name)))
    (save-buffer)))

(add-hook 'gptel-post-response-functions #'gptel-session--auto-save)

(provide 'gptel-sessions)
;;; gptel-sessions.el ends here
