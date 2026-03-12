;;; gptel-tools.el --- Tool-based skills for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal, focused tools for gptel with magit and projectile integration

;;; Code:

(require 'gptel)
(require 'magit)
(require 'projectile)

;;; Git Safety Helpers

(defvar gptel-tool-branch-prefix "gptel-work/"
  "Prefix for branches created by gptel.")

(defun gptel-tool--ensure-git ()
  "Ensure git repo exists, create if needed. Returns root."
  (or (magit-toplevel)
      (let* ((default-root (or (projectile-project-root) default-directory))
             (root (read-directory-name "Initialize git at: " default-root default-root)))
        (when (y-or-n-p (format "Create git repo at %s? " root))
          (let ((default-directory root))
            (magit-init root)
            (with-temp-file (expand-file-name ".gitignore" root)
              (insert "*~\n*.elc\n"))
            (magit-stage-files ".gitignore")
            (magit-commit-create '("-m" "Initial commit"))
            (unless (member root (projectile-relevant-known-projects))
              (projectile-add-known-project root))
            root)))))

(defun gptel-tool--ensure-branch ()
  "Ensure on gptel branch, create if needed. Stash uncommitted changes."
  (let* ((root (gptel-tool--ensure-git))
         (default-directory root)
         (branch (magit-get-current-branch)))
    (unless (string-prefix-p gptel-tool-branch-prefix branch)
      ;; Stash uncommitted changes if any
      (when (magit-anything-modified-p)
        (let ((stash-msg (format "gptel-tool stash %s"
                                (format-time-string "%Y-%m-%d %H:%M:%S"))))
          (magit-stash-both stash-msg)
          (message "Stashed changes: %s" stash-msg)))
      ;; Create and checkout new branch
      (let ((new-branch (format "%s%s" gptel-tool-branch-prefix
                               (format-time-string "%Y%m%d-%H%M%S"))))
        (magit-branch-and-checkout new-branch branch)
        (message "Created branch: %s" new-branch)
        new-branch))))

(defun gptel-tool--project-path (path)
  "Expand PATH relative to project root."
  (expand-file-name path (gptel-tool--ensure-git)))

;;; Tool Definitions

;; Read file
(defun gptel-tool-read-file (path line-numbers)
  "Read file at PATH (relative to project root).
If LINE-NUMBERS is non-nil, prefix each line with its line number."
  (let ((file (gptel-tool--project-path path)))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (if line-numbers
              (let ((line-num 1)
                    (result ""))
                (goto-char (point-min))
                (while (not (eobp))
                  (setq result (concat result
                                      (format "%4d | %s\n"
                                             line-num
                                             (buffer-substring (line-beginning-position)
                                                             (line-end-position)))))
                  (setq line-num (1+ line-num))
                  (forward-line 1))
                result)
            (buffer-string)))
      (error "File not found: %s" path))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-read-file
              :name "read_file"
              :description "Read file contents, optionally with line numbers"
              :args '((:name "path" :type string :description "File path relative to project root")
                      (:name "line_numbers" :type boolean :description "Show line numbers" :optional t))
              :category "filesystem"))

;; List directory
(defun gptel-tool-list-dir (dir recursive)
  "List contents of DIR, optionally RECURSIVE."
  (let* ((path (gptel-tool--project-path dir))
         (files (if recursive
                   (directory-files-recursively path ".*")
                 (directory-files path t "^[^.]")))
         (root (or (projectile-project-root) default-directory)))
    (mapconcat (lambda (f) (file-relative-name f root)) files "\n")))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-list-dir
              :name "list_dir"
              :description "List directory contents"
              :args '((:name "dir" :type string :description "Directory path")
                      (:name "recursive" :type boolean :description "Recurse into subdirs" :optional t))
              :category "filesystem"))

;; Edit file
(defun gptel-tool-edit-file (path content start-line end-line)
  "Edit file at PATH. If START-LINE and END-LINE provided, replace those lines.
Otherwise, replace entire file with CONTENT."
  (gptel-tool--ensure-branch)
  (let ((file (gptel-tool--project-path path)))
    (if (and start-line end-line)
        ;; Partial edit
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (forward-line (1- start-line))
          (let ((start (point)))
            (forward-line (1+ (- end-line start-line)))
            (delete-region start (point)))
          (insert content)
          (write-region nil nil file))
      ;; Full file replacement
      (with-temp-file file (insert content)))

    (when (projectile-project-root)
      (projectile-invalidate-cache nil))

    (if (and start-line end-line)
        (format "Edited lines %d-%d in %s" start-line end-line path)
      (format "Wrote %d bytes to %s" (length content) path))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-edit-file
              :name "edit_file"
              :description "Edit file: replace entire file or specific line range"
              :args '((:name "path" :type string :description "File path")
                      (:name "content" :type string :description "New content")
                      (:name "start_line" :type integer :description "Start line (1-indexed)" :optional t)
                      (:name "end_line" :type integer :description "End line (inclusive)" :optional t))
              :confirm t
              :category "filesystem"))


;; Write file
(defun gptel-tool-write-file (path content)
  "Write CONTENT to file at PATH."
  (gptel-tool--ensure-branch)
  (let ((file (gptel-tool--project-path path)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert content))
    (when (projectile-project-root)
      (projectile-invalidate-cache nil))
    (format "Wrote %d bytes to %s" (length content) path)))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-write-file
              :name "write_file"
              :description "Write content to file"
              :args '((:name "path" :type string :description "File path")
                      (:name "content" :type string :description "File content"))
              :confirm t
              :category "filesystem"))

;; Git commit
(defun gptel-tool-git-commit (msg)
  "Stage and commit all changes with MSG."
  (let ((default-directory (gptel-tool--ensure-git)))
    (magit-stage-modified)
    (magit-commit-create (list "-m" msg))
    "Committed"))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-git-commit
              :name "git_commit"
              :description "Stage and commit all changes"
              :args '((:name "msg" :type string :description "Commit message"))
              :confirm t
              :category "git"))

;; Git status
(defun gptel-tool-git-status ()
  "Get git repository status."
  (if-let ((root (gptel-tool--ensure-git)))
      (let ((default-directory root))
        (format "%s | %s | M:%d S:%d"
                (magit-get-current-branch)
                (if (magit-anything-modified-p) "dirty" "clean")
                (length (magit-modified-files))
                (length (magit-staged-files))))
    "No git repo"))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-git-status
              :name "git_status"
              :description "Get git status"
              :args nil
              :category "git"))

;; Git diff
(defun gptel-tool-git-diff (staged)
  "Show git diff. If STAGED is non-nil, show staged changes."
  (let ((default-directory (gptel-tool--ensure-git)))
    (shell-command-to-string
     (if staged "git diff --staged" "git diff"))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-git-diff
              :name "git_diff"
              :description "Show git diff"
              :args '((:name "staged" :type boolean :description "Show staged changes" :optional t))
              :category "git"))

;; Project files
(defun gptel-tool-list-project (pattern)
  "List project files, optionally filtered by PATTERN."
  (let ((files (if pattern
                  (projectile-find-matching-file pattern)
                (projectile-current-project-files))))
    (if (> (length files) 100)
        (format "%s\n... (%d more)"
               (string-join (seq-take files 100) "\n")
               (- (length files) 100))
      (string-join files "\n"))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-list-project
              :name "list_project"
              :description "List project files"
              :args '((:name "pattern" :type string :description "Filter pattern" :optional t))
              :category "project"))

;; Project structure
(defun gptel-tool-show-tree (depth)
  "Show directory tree up to DEPTH levels using projectile."
  (let* ((root (or (projectile-project-root) default-directory))
         (default-directory root)
         (max-depth (or depth 2))
         ;; Use projectile's cached files or force a clean retrieval
         (files (let ((inhibit-message t)
                      (message-log-max nil))
                  (projectile-project-files root)))
         (tree-items '()))

    ;; Filter files by depth
    (dolist (file files)
      (let* ((parts (split-string file "/"))
             (file-depth (length parts)))
        (when (<= file-depth max-depth)
          (push file tree-items))))

    (setq tree-items (sort tree-items #'string<))

    (format "Project: %s\n%s\n\nFiles (%d shown, depth <= %d):\n%s"
            (projectile-project-name)
            root
            (length tree-items)
            max-depth
            (mapconcat
             (lambda (file)
               (let* ((parts (split-string file "/"))
                      (indent (make-string (* 2 (1- (length parts))) ?\s)))
                 (format "%s%s" indent (car (last parts)))))
             tree-items
             "\n"))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-show-tree
              :name "show_tree"
              :description "Show directory tree"
              :args '((:name "depth" :type integer :description "Max depth (default: 2)" :optional t))
              :category "project"))

;;; Memory Management Integration

(defun gptel-tool--check-session-size ()
  "Check current session size and memory context size.
Returns a plist with :session-tokens and :memory-tokens."
  (let ((session-tokens 0)
        (memory-tokens 0))
    ;; Calculate session buffer tokens
    (when (buffer-file-name)
      (setq session-tokens (gptel-memory--estimate-tokens (buffer-string))))

    ;; Calculate memory context tokens
    (when (fboundp 'gptel-memory--context-file)
      (let ((context-file (gptel-memory--context-file)))
        (when (file-exists-p context-file)
          (with-temp-buffer
            (insert-file-contents context-file)
            (setq memory-tokens (gptel-memory--estimate-tokens (buffer-string)))))))

    (list :session-tokens session-tokens
          :memory-tokens memory-tokens
          :total-tokens (+ session-tokens memory-tokens))))

(defun gptel-tool--maybe-prompt-summarization ()
  "Check if summarization is needed and prompt user."
  (when (and (fboundp 'gptel-memory-check-size)
             gptel-memory-auto-update)
    (let ((size-info (gptel-tool--check-session-size)))
      (when (gptel-memory-check-size)
        (message "Memory context approaching limit (%d tokens). Consider summarization."
                 (plist-get size-info :memory-tokens))
        (when (y-or-n-p "Context exceeds threshold. Summarize now? ")
          (gptel-memory-request-summary))))))

;; Hook to check memory after tool execution
(defun gptel-tool--post-tool-check (&rest _)
  "Check memory size after tool execution."
  (when (and gptel-mode
             (featurep 'gptel-memory))
    (run-with-idle-timer 0.5 nil #'gptel-tool--maybe-prompt-summarization)))

;; Add hook to gptel-post-response-functions if not already there
(unless (member 'gptel-tool--post-tool-check gptel-post-response-functions)
  (add-hook 'gptel-post-response-functions #'gptel-tool--post-tool-check))

;; Memory status tool
(defun gptel-tool-memory-status ()
  "Get current memory and session status."
  (require 'gptel-memory)
  (let* ((size-info (gptel-tool--check-session-size))
         (context-file (gptel-memory--context-file))
         (threshold (* gptel-memory-max-tokens gptel-memory-summarize-threshold))
         (memory-tokens (plist-get size-info :memory-tokens))
         (percentage (if (> memory-tokens 0)
                        (* 100 (/ (float memory-tokens) gptel-memory-max-tokens))
                      0)))
    (format "Memory Status:
- Context file: %s
- Session tokens: %d
- Memory tokens: %d (%.1f%% of max)
- Total tokens: %d
- Threshold: %d tokens
- Summarization %s"
            (if (file-exists-p context-file) "exists" "not found")
            (plist-get size-info :session-tokens)
            memory-tokens
            percentage
            (plist-get size-info :total-tokens)
            threshold
            (if (> memory-tokens threshold) "RECOMMENDED" "not needed"))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-memory-status
              :name "memory_status"
              :description "Check current memory and context size status"
              :args nil
              :category "memory"))

;; Memory update tool
(defun gptel-tool-update-memory (section content)
  "Update a SECTION in memory context with CONTENT."
  (require 'gptel-memory)
  (let ((context-file (gptel-memory--context-file)))
    (unless (file-exists-p context-file)
      (gptel-memory-init))

    (with-temp-buffer
      (insert-file-contents context-file)
      (goto-char (point-min))

      (if (re-search-forward (format "^\\* %s" (regexp-quote section)) nil t)
          ;; Update existing section
          (let ((start (line-beginning-position))
                (end (or (and (re-search-forward "^\\* " nil t)
                             (line-beginning-position))
                        (point-max))))
            (delete-region start end)
            (goto-char start)
            (insert (format "* %s\n\n%s\n\n" section content)))
        ;; Add new section
        (goto-char (point-max))
        (insert (format "\n* %s\n\n%s\n" section content)))

      (write-region nil nil context-file))

    ;; Check if we need summarization after update
    (gptel-tool--maybe-prompt-summarization)

    (format "Updated memory section '%s' (%d tokens)"
            section
            (gptel-memory--estimate-tokens content))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :function #'gptel-tool-update-memory
              :name "update_memory"
              :description "Update a section in the memory context"
              :args '((:name "section" :type string :description "Section name (e.g., 'Current Task', 'Key Decisions')")
                      (:name "content" :type string :description "Content to add/update"))
              :confirm t
              :category "memory"))

(provide 'gptel-tools)
;;; gptel-tools.el ends here
