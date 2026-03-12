;;; gptel-sessions.el --- Session management for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatic session saving and management for gptel with project integration

;;; Code:

(require 'gptel)
(require 'gptel-tools)

;;; Configuration

(defvar gptel-session-directory ".gptel"
  "Directory name for storing gptel sessions relative to project root.")

;;; Helper Functions

(defun gptel-session--get-dir ()
  "Get or create .gptel directory in project root."
  (let* ((project-root (gptel-tool--ensure-git))
         (session-dir (expand-file-name gptel-session-directory project-root)))
    (unless (file-exists-p session-dir)
      (make-directory session-dir t))
    session-dir))

(defun gptel-session--generate-name ()
  "Generate a session filename based on timestamp."
  (format "session-%s.org" (format-time-string "%Y%m%d-%H%M%S")))

;;; Interactive Commands

(defun gptel-session-start (&optional name)
  "Start a gptel session in project's .gptel directory.
If NAME is provided, use it as the filename, otherwise generate one."
  (interactive)
  (let* ((session-dir (gptel-session--get-dir))
         (session-name (or name (gptel-session--generate-name)))
         (session-file (expand-file-name session-name session-dir))
         (project-root (gptel-tool--ensure-git))
         (project-name (or (and (projectile-project-root)
                                (not (string= (projectile-project-name) "-"))
                                (projectile-project-name))
                          (file-name-nondirectory 
                           (directory-file-name project-root))
                          "default")))
    (message "Starting gptel session: %s (project: %s, root: %s)"
             session-name project-name project-root)
    ;; Create or switch to session buffer
    (with-current-buffer (find-file session-file)
      ;; Set as org-mode and enable gptel-mode
      (unless (derived-mode-p 'org-mode)
        (org-mode))
      (unless gptel-mode
        (gptel-mode 1))

      ;; Set buffer-local default-directory to project root
      (setq-local default-directory project-root)

      ;; Add header if new file
      (when (= (buffer-size) 0)
        (insert (format "#+TITLE: GPtel Session - %s\n" project-name))
        (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "#+PROPERTY: PROJECT_ROOT %s\n\n" project-root))
        (insert (alist-get 'org-mode gptel-prompt-prefix-alist)))

      (goto-char (point-max))
      (message "GPtel session: %s (working dir: %s)" session-name project-root)
      (current-buffer))))

(defun gptel-session-resume ()
  "Resume an existing gptel session from project's .gptel directory."
  (interactive)
  (let* ((session-dir (gptel-session--get-dir))
         (sessions (directory-files session-dir nil "^session-.*\\.org$"))
         (project-root (or (projectile-project-root) default-directory)))
    (if sessions
        (let* ((session (completing-read "Resume session: " sessions))
               (session-file (expand-file-name session session-dir)))
          (with-current-buffer (find-file session-file)
            (unless gptel-mode (gptel-mode 1))
            ;; Restore project root from property or default
            (setq-local default-directory
                        (or (org-entry-get nil "PROJECT_ROOT" t)
                            project-root))
            (goto-char (point-max))
            (current-buffer)))
      (message "No sessions found in %s" session-dir)
      (when (y-or-n-p "Start a new session? ")
        (gptel-session-start)))))

(defun gptel-session-list ()
  "List all gptel sessions in current project."
  (interactive)
  (let* ((session-dir (gptel-session--get-dir))
         (sessions (directory-files session-dir nil "^session-.*\\.org$")))
    (if sessions
        (message "Sessions in %s:\n%s"
                 session-dir
                 (mapconcat #'identity sessions "\n"))
      (message "No sessions found in %s" session-dir))))

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
             (string-match-p "/\\.gptel/session-.*\\.org$" (buffer-file-name)))
        ;; Already in a session, just ensure gptel-mode
        (unless gptel-mode (gptel-mode 1))
      ;; Check for existing sessions and prompt to resume
      (let* ((session-dir (gptel-session--get-dir))
             (sessions (directory-files session-dir nil "^session-.*\\.org$")))
        (if (and sessions
                 (y-or-n-p (format "Found %d existing session(s). Resume one? "
                                   (length sessions))))
            (gptel-session-resume)
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
