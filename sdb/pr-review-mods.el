;;; pr-review-mods.el --- Add functionality to pr-review using forge branches -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom functions to ediff PR files using forge to checkout the PR branch locally
;; This approach uses forge-branch-pullreq to create a local branch of the PR

;;; Code:
(require 'forge)
(require 'magit)
(require 'pr-review)

;; Variable to store the git directory for the current PR
(defvar-local pr-review--local-git-dir nil
  "Local git directory for the current pr-review buffer.")

;; Variable to store the PR branch name
(defvar-local pr-review--pr-branch-name nil
  "Local branch name for the current PR.")

;; Variable to store the PR base branch name
(defvar-local pr-review--pr-base-branch-name nil
  "Base branch name that the PR targets (e.g., main, master, develop).")

;; Hook to capture git directory when pr-review-mode is activated
(defun my/pr-review-capture-git-dir ()
  "Capture the git directory for the current pr-review buffer."
  (unless pr-review--local-git-dir
    (setq pr-review--local-git-dir
          (or
           (when (and default-directory (file-directory-p default-directory))
             (locate-dominating-file default-directory ".git"))
           (when-let* ((owner (nth 0 pr-review--pr-path))
                       (repo (nth 1 pr-review--pr-path))
                       (base-dir (expand-file-name
                                 (format "%s/%s" owner repo)
                                 my/pr-review-repo-base-dir))
                       (git-dir (locate-dominating-file base-dir ".git")))
             git-dir)))))

(add-hook 'pr-review-mode-hook #'my/pr-review-capture-git-dir)

;; Advice to capture git directory when opening PR from magit
(defun my/pr-review-capture-git-dir-advice (&rest _args)
  "Capture the current git directory before opening pr-review."
  (when-let ((git-dir (and (or (derived-mode-p 'magit-mode)
                               (derived-mode-p 'magit-status-mode))
                           (magit-toplevel))))
    (setq my/pr-review--pending-git-dir git-dir)))

(defvar my/pr-review--pending-git-dir nil
  "Temporary storage for git directory when opening a PR.")

(advice-add 'pr-review :before #'my/pr-review-capture-git-dir-advice)

;; Enhanced hook to use the pending git dir
(defun my/pr-review-set-pending-git-dir ()
  "Set the git directory from pending storage if available."
  (when my/pr-review--pending-git-dir
    (setq-local pr-review--local-git-dir my/pr-review--pending-git-dir)
    (setq my/pr-review--pending-git-dir nil)))

(add-hook 'pr-review-mode-hook #'my/pr-review-set-pending-git-dir)

;; Customizable base directory
(defcustom my/pr-review-repo-base-dir "~/repos"
  "Base directory where git repositories are stored."
  :type 'directory
  :group 'pr-review)

(defcustom my/pr-review-base-branch-name "main"
  "Fallback name of the base branch to compare against.
Common values are 'main', 'master', 'trunk', 'develop'.
This is only used if the base branch cannot be determined from the PR."
  :type 'string
  :group 'pr-review)

(defun my/pr-review-get-git-dir ()
  "Get the git directory for the current pr-review buffer."
  (or pr-review--local-git-dir
      (progn
        (my/pr-review-capture-git-dir)
        pr-review--local-git-dir)))

(defun my/pr-review-get-pr-number ()
  "Get the PR number from the current pr-review buffer."
  (when pr-review--pr-path
    (nth 2 pr-review--pr-path)))

(defun my/pr-review-get-pr-branch-name ()
  "Get the cached local branch name for the current PR, if it exists."
  pr-review--pr-branch-name)

(defun my/pr-review-get-pr-base-branch ()
  "Get the base branch that the PR targets.
Returns the cached value if available, otherwise queries forge."
  (or pr-review--pr-base-branch-name
      (let* ((git-dir (my/pr-review-get-git-dir))
             (pr-number (my/pr-review-get-pr-number)))
        (when (and git-dir pr-number)
          (let ((default-directory git-dir))
            (when (fboundp 'forge-get-repository)
              (when-let* ((repo (forge-get-repository :tracked))
                         (pullreq (forge-get-pullreq repo pr-number))
                         (base-ref (and pullreq (oref pullreq base-ref))))
                (setq pr-review--pr-base-branch-name base-ref)
                base-ref)))))))

(defun my/pr-review-ensure-pr-branch ()
  "Ensure the PR branch exists locally, creating it with forge if needed.
Does not check out the branch - leaves that to individual functions.
Returns the branch name."
  (let* ((git-dir (my/pr-review-get-git-dir))
         (pr-number (my/pr-review-get-pr-number))
         (branch-name (my/pr-review-get-pr-branch-name)))

    (unless git-dir
      (user-error "Git directory not found"))

    (unless pr-number
      (user-error "Could not determine PR number"))

    (let ((default-directory git-dir))
      (unless (fboundp 'forge-branch-pullreq)
        (user-error "Forge is not installed. Please install magit-forge"))

      (let* ((repo (forge-get-repository :tracked))
             (pullreq (forge-get-pullreq repo pr-number))
             (remote-branch (and pullreq (oref pullreq head-ref)))
             (local-branch-exists (and remote-branch
                                       (member remote-branch (magit-list-local-branch-names)))))

        (unless pullreq
          (user-error "PR #%s not found in forge database. Try running `forge-pull' first" pr-number))

        (unless remote-branch
          (user-error "Could not determine remote branch name for PR #%s" pr-number))

        ;; Cache the base branch from the PR
        (when pullreq
          (setq pr-review--pr-base-branch-name (oref pullreq base-ref)))

        (cond
         ((and local-branch-exists (string= branch-name remote-branch))
          (message "PR branch already exists locally: %s" remote-branch)
          (setq branch-name remote-branch))

         ((and local-branch-exists (not (string= branch-name remote-branch)))
          (message "Local branch exists but doesn't match stored name. Using: %s" remote-branch)
          (setq branch-name remote-branch)
          (setq pr-review--pr-branch-name branch-name))

         (t
          (message "Creating local branch for PR #%s using forge..." pr-number)
          (let ((original-branch (magit-get-current-branch)))
            (forge-branch-pullreq pullreq)
            ;; forge-branch-pullreq checks out the new branch, so switch back
            (when (and original-branch
                      (not (string= original-branch remote-branch)))
              (magit--checkout original-branch)))
          (setq branch-name remote-branch)
          (setq pr-review--pr-branch-name branch-name)
          (unless branch-name
            (user-error "Failed to create PR branch"))
          (message "Created PR branch: %s" branch-name)))))

    (message "Using PR branch: %s" branch-name)
    branch-name))

(defun my/pr-review-get-file-path-at-point ()
  "Get the file path at point in pr-review buffer."
  (or
   ;; Try to get filename from diff line properties
   (when-let ((left-prop (get-text-property (point) 'pr-review-diff-line-left)))
     (car left-prop))
   (when-let ((right-prop (get-text-property (point) 'pr-review-diff-line-right)))
     (car right-prop))
   ;; Try to get from magit-section
   (when-let ((section (get-text-property (point) 'magit-section)))
     (cond
      ;; If we're on a file section, use it directly
      ((magit-file-section-p section)
       (oref section value))
      ;; If we're in a hunk or any other subsection, traverse up to find the file section
      (t
       (let ((parent section))
         (while (and parent (not (magit-file-section-p parent)))
           (setq parent (oref parent parent)))
         (when (and parent (magit-file-section-p parent))
           (oref parent value))))))))

(defun my/pr-review-ediff-with-base ()
  "Ediff the file at point between the PR branch and base branch.
If base branch doesn't exist locally, prompts for branch selection.
Creates the PR branch locally using forge if it doesn't exist."
  (interactive)
  (unless (derived-mode-p 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))

  (let* ((git-dir (my/pr-review-get-git-dir))
         (default-directory (or git-dir default-directory))
         (base-branch (or (my/pr-review-get-pr-base-branch)
                         my/pr-review-base-branch-name))
         (branch-exists (and git-dir
                            (zerop (call-process "git" nil nil nil
                                               "rev-parse" "--verify"
                                               (concat "refs/heads/" base-branch))))))

    (if branch-exists
        ;; Base branch exists, use it directly
        (my/pr-review-ediff-with-branch base-branch)
      ;; Base branch doesn't exist, ask user to select a branch
      (progn
        (message "Branch '%s' not found in local repository" base-branch)
        (call-interactively #'my/pr-review-ediff-with-branch)))))

(defun my/pr-review-ediff-with-branch (branch)
  "Ediff the file at point between the PR branch and a specified BRANCH.
Creates the PR branch locally using forge if it doesn't exist."
  (interactive
   (list (let* ((git-dir (my/pr-review-get-git-dir))
                (default-directory (or git-dir default-directory))
                (base-branch (or (my/pr-review-get-pr-base-branch)
                                my/pr-review-base-branch-name))
                (branches (and git-dir
                              (magit-list-local-branch-names))))
           (if branches
               (completing-read "Compare PR with branch: "
                              branches
                              nil    ; predicate
                              t      ; require-match
                              nil    ; initial-input
                              nil    ; hist
                              base-branch) ; default
             (read-string "Compare PR with branch: " base-branch)))))

  (unless (derived-mode-p 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))

  (let* ((file-path (my/pr-review-get-file-path-at-point))
         (git-dir (my/pr-review-get-git-dir))
         (pr-branch (my/pr-review-ensure-pr-branch)))

    (unless file-path
      (user-error "No file at point"))

    (unless git-dir
      (user-error "Git directory not found"))

    (message "Comparing %s between %s and %s..." file-path pr-branch branch)

    (let ((default-directory git-dir))
      (let* ((pr-content
              (with-temp-buffer
                (let ((exit-code (call-process "git" nil t nil
                                              "show"
                                              (format "%s:%s" pr-branch file-path))))
                  (when (not (zerop exit-code))
                    (user-error "Failed to get file %s from branch %s. File may not exist in PR"
                               file-path pr-branch)))
                (buffer-string)))
             (pr-buffer (generate-new-buffer
                        (format "*%s:%s*" pr-branch (file-name-nondirectory file-path))))

             ;; Try to get branch version, handling new files gracefully
             (branch-content
              (with-temp-buffer
                (let ((exit-code (call-process "git" nil t nil
                                              "show"
                                              (format "%s:%s" branch file-path))))
                  (if (zerop exit-code)
                      (buffer-string)
                    ;; File doesn't exist on comparison branch - return empty string
                    (progn
                      (message "File %s is new in PR (doesn't exist on %s)"
                              file-path branch)
                      "")))))
             (branch-buffer (generate-new-buffer
                            (format "*%s:%s*" branch (file-name-nondirectory file-path)))))

        (with-current-buffer pr-buffer
          (insert pr-content)
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (let ((mode (assoc-default file-path auto-mode-alist 'string-match)))
            (when mode (funcall mode))))

        (with-current-buffer branch-buffer
          (insert branch-content)
          (when (string-empty-p branch-content)
            ;; Add a comment indicating this is a new file
            (insert (format ";; File %s is new in this PR\n" file-path))
            (insert ";; This branch does not contain this file\n"))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (let ((mode (assoc-default file-path auto-mode-alist 'string-match)))
            (when mode (funcall mode))))

        (ediff-buffers branch-buffer pr-buffer)))))

(defun my/pr-review-show-branch-info ()
  "Show git directory and PR branch information."
  (interactive)
  (let ((git-dir (my/pr-review-get-git-dir))
        (pr-number (my/pr-review-get-pr-number))
        (pr-branch (my/pr-review-get-pr-branch-name))
        (base-branch (my/pr-review-get-pr-base-branch)))
    (message "Git dir: %s | PR: #%s | Branch: %s -> %s"
             (or git-dir "not found")
             (or pr-number "unknown")
             (or pr-branch "not checked out")
             (or base-branch "unknown"))))

(defun my/pr-review-visit-file ()
  "Visit the file at point from the PR branch in a new buffer.
If point is on a modified line in a hunk, jump to that line in the file.
If the file is already visible in the current frame, focuses that window.
Handles renamed files by checking magit-section properties."
  (interactive)
  (unless (derived-mode-p 'pr-review-mode)
    (user-error "Not in a pr-review buffer"))

  (let* ((diff-info (pr-review--get-diff-line-info (point)))
         (diff-side (car diff-info))  ; "LEFT" or "RIGHT"
         (file-path (car (cdr diff-info)))  ; filename from (filename . line)
         (target-line (cdr (cdr diff-info)))  ; line from (filename . line)
         (is-removed-line (equal diff-side "LEFT"))
         (section (get-text-property (point) 'magit-section))
         (file-section (when section
                         (let ((parent section))
                           (while (and parent (not (magit-file-section-p parent)))
                             (setq parent (oref parent parent)))
                           parent)))
         ;; Check if the file was renamed by looking at magit-section properties
         (renamed-p (and file-section
                        (slot-boundp file-section 'source)
                        (oref file-section source)))
         ;; For removed lines in renamed files, use the original name
         (actual-file-path (if (and is-removed-line renamed-p)
                               (oref file-section source)
                             file-path))
         (git-dir (my/pr-review-get-git-dir))
         (pr-branch (my/pr-review-ensure-pr-branch))
         (base-branch (or (my/pr-review-get-pr-base-branch)
                         my/pr-review-base-branch-name))
         (target-branch (if is-removed-line
                            base-branch
                          pr-branch)))

    (unless actual-file-path
      (user-error "No file at point"))

    (unless git-dir
      (user-error "Git directory not found"))

    (message "file-path at point: %s" file-path)
    (message "actual-file-path: %s" actual-file-path)
    (message "is-removed-line: %s" is-removed-line)
    (message "target-line is %s in branch %s" target-line target-branch)

    (when renamed-p
      (message "File renamed: %s -> %s"
               (oref file-section source)
               (oref file-section value)))

    (let ((default-directory git-dir))
      (let ((current-branch (magit-get-current-branch)))
        (unless (string= current-branch target-branch)
          (magit--checkout target-branch)
          (magit-refresh)
          (unless (string= (magit-get-current-branch) target-branch)
            (user-error "Failed to checkout branch %s" target-branch)))))

    (let* ((full-path (expand-file-name actual-file-path git-dir))
           (file-buffer (find-file-noselect full-path))
           (window (get-buffer-window file-buffer (selected-frame))))
      (if window
          (select-window window)
        (progn
          (if (one-window-p t (selected-frame))
              (progn
                (split-window-right)
                (other-window 1)
                (switch-to-buffer file-buffer))
            (other-window 1)
            (switch-to-buffer file-buffer))))

      (when target-line
        (goto-char (point-min))
        (forward-line (1- target-line))
        (recenter)
        (message "Jumped to line %d in %s on branch %s" target-line actual-file-path target-branch)))))

(defun my/pr-review--find-file-and-lines-in-pr (pr-buffer file-path start-line end-line)
  "Find FILE-PATH and line range in PR-BUFFER's diff.
START-LINE and END-LINE specify the range to find.
Returns a plist with:
  :found-file - t if file was found
  :file-start - position of file section start
  :file-end - position of file section end
  :start-pos - position of first line in range (or closest)
  :end-pos - position of last line in range (or closest)
  :exact-match - t if exact line range was found
  :closest-start - actual line number at start-pos
  :closest-end - actual line number at end-pos"
  (with-current-buffer pr-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((found-file nil)
            (file-start nil)
            (file-end nil)
            (start-pos nil)
            (end-pos nil)
            (exact-start nil)
            (exact-end nil)
            (closest-start-pos nil)
            (closest-end-pos nil)
            (closest-start-line nil)
            (closest-end-line nil)
            (closest-start-distance most-positive-fixnum)
            (closest-end-distance most-positive-fixnum))

        ;; Search for the file section
        (while (and (not found-file)
                   (not (eobp)))
          (when-let ((section (get-text-property (point) 'magit-section)))
            (when (and (magit-file-section-p section)
                      (string= (oref section value) file-path))
              (setq found-file t)
              (setq file-start (point))
              (setq file-end (oref section end))

              ;; Search for lines in the diff
              (while (< (point) file-end)
                (when-let ((right-prop (get-text-property (point) 'pr-review-diff-line-right)))
                  (let ((diff-line (cdr right-prop)))

                    ;; Check for exact match at start
                    (when (= diff-line start-line)
                      (setq exact-start t)
                      (setq start-pos (point)))

                    ;; Check for exact match at end (or use start if single line)
                    (when (= diff-line (or end-line start-line))
                      (setq exact-end t)
                      (setq end-pos (point)))

                    ;; Track closest to start-line
                    (let ((distance (abs (- diff-line start-line))))
                      (when (< distance closest-start-distance)
                        (setq closest-start-distance distance)
                        (setq closest-start-pos (point))
                        (setq closest-start-line diff-line)))

                    ;; Track closest to end-line
                    (when end-line
                      (let ((distance (abs (- diff-line end-line))))
                        (when (< distance closest-end-distance)
                          (setq closest-end-distance distance)
                          (setq closest-end-pos (point))
                          (setq closest-end-line diff-line))))))

                (forward-line 1))))
          (forward-line 1))

        ;; Use closest matches if exact not found
        (unless start-pos
          (setq start-pos closest-start-pos))
        (unless end-pos
          (setq end-pos (or closest-end-pos closest-start-pos)))

        (list :found-file found-file
              :file-start file-start
              :file-end file-end
              :start-pos start-pos
              :end-pos end-pos
              :exact-match (and exact-start (or exact-end (not end-line)))
              :closest-start (or closest-start-line start-line)
              :closest-end (or closest-end-line end-line start-line))))))

(defun my/pr-review--get-pr-buffer (&optional file-path)
  "Get the PR review buffer to use.
If FILE-PATH is provided, checks if it exists in any open PR and returns that buffer.
If multiple PR buffers exist and FILE-PATH doesn't narrow it down, prompts user to choose.
Returns the buffer or signals an error if none exist."
  (let ((pr-review-buffers (cl-remove-if-not
                           (lambda (buf)
                             (with-current-buffer buf
                               (eq major-mode 'pr-review-mode)))
                           (buffer-list))))

    (unless pr-review-buffers
      (user-error "No pr-review buffers found. Open a PR first with M-x pr-review"))

    (cond
     ;; Only one PR buffer - use it
     ((= (length pr-review-buffers) 1)
      (car pr-review-buffers))

     ;; Multiple buffers and we have a file path - check which PR contains the file
     ((and file-path (> (length pr-review-buffers) 1))
      (let ((matching-buffer
             (cl-find-if
              (lambda (buf)
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (let ((found nil))
                      (while (and (not found) (not (eobp)))
                        (when-let ((section (get-text-property (point) 'magit-section)))
                          (when (and (magit-file-section-p section)
                                    (string= (oref section value) file-path))
                            (setq found t)))
                        (forward-line 1))
                      found))))
              pr-review-buffers)))
        (if matching-buffer
            matching-buffer
          ;; File not found in any PR, ask user
          (get-buffer
           (completing-read "File not in any open PR. Choose PR review buffer: "
                          (mapcar #'buffer-name pr-review-buffers)
                          nil t)))))

     ;; Multiple buffers, no file path - ask user
     (t
      (get-buffer
       (completing-read "Jump to PR review: "
                       (mapcar #'buffer-name pr-review-buffers)
                       nil t))))))

(defun my/pr-review--switch-to-buffer (buffer)
  "Switch to BUFFER in another window within the current frame.
If the buffer is already displayed in a window, select that window.
Otherwise, if another window exists in the frame, switch to it and display the buffer.
If only one window exists, split vertically and display the buffer in the new window."
  (let ((window (get-buffer-window buffer (selected-frame))))
    (if window
        (select-window window)
      (if (one-window-p t (selected-frame))
          (progn
            (split-window-right)
            (other-window 1)
            (switch-to-buffer buffer))
        (other-window 1)
        (switch-to-buffer buffer)))))

(defun my/pr-review-jump-to-file-in-pr ()
  "Jump from current file buffer back to the PR review buffer.
Jumps to the line in the diff closest to the current line position.
If the file exists in an already-open PR buffer, automatically uses that buffer.
If the PR buffer is already visible in the current frame, focuses that window."
  (interactive)

  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))

  (let* ((current-file (buffer-file-name))
         (current-line (line-number-at-pos))
         (git-dir (locate-dominating-file current-file ".git"))
         (relative-path (and git-dir
                            (file-relative-name current-file git-dir))))

    (unless git-dir
      (user-error "Not in a git repository"))

    (let* ((target-buffer (my/pr-review--get-pr-buffer relative-path))
           (result (my/pr-review--find-file-and-lines-in-pr
                   target-buffer relative-path current-line nil)))

      (my/pr-review--switch-to-buffer target-buffer)

      (cond
       ;; File not found in PR
       ((not (plist-get result :found-file))
        (message "File %s not found in current PR" relative-path))

       ;; Exact match found
       ((plist-get result :exact-match)
        (goto-char (plist-get result :start-pos))
        (recenter)
        (message "Jumped to line %d in PR diff for %s"
                current-line relative-path))

       ;; Closest match found
       ((plist-get result :start-pos)
        (goto-char (plist-get result :start-pos))
        (recenter)
        (message "Line %d not in diff. Jumped to closest modified line %d in %s"
                current-line
                (plist-get result :closest-start)
                relative-path))

       ;; File found but no modified lines
       (t
        (goto-char (plist-get result :file-start))
        (recenter)
        (message "Jumped to file %s (no lines modified near line %d)"
                relative-path current-line))))))

(defun my/pr-review-comment-on-region ()
  "Create a PR review comment for the selected region in the local buffer.
Jumps to the PR review buffer and starts a comment on the corresponding lines.
If the region spans multiple lines, creates a multi-line comment.
If the file exists in an already-open PR buffer, automatically uses that buffer.
If the PR buffer is already visible in the current frame, focuses that window."
  (interactive)

  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))

  (unless (use-region-p)
    (user-error "No region selected. Select the lines you want to comment on"))

  (let* ((current-file (buffer-file-name))
         (region-start (region-beginning))
         (region-end (region-end))
         (start-line (line-number-at-pos region-start))
         (end-line (line-number-at-pos region-end))
         (git-dir (locate-dominating-file current-file ".git"))
         (relative-path (and git-dir
                            (file-relative-name current-file git-dir))))

    (unless git-dir
      (user-error "Not in a git repository"))

    (let* ((target-buffer (my/pr-review--get-pr-buffer relative-path))
           (result (my/pr-review--find-file-and-lines-in-pr
                   target-buffer relative-path start-line end-line)))

      (my/pr-review--switch-to-buffer target-buffer)

      (cond
       ;; File not found
       ((not (plist-get result :found-file))
        (user-error "File %s not found in current PR" relative-path))

       ;; Lines not found in diff
       ((not (plist-get result :start-pos))
        (goto-char (plist-get result :file-start))
        (recenter)
        (user-error "Lines %d-%d not found in PR diff (may not be modified)"
                   start-line end-line))

       ;; Multi-line region
       ((and (plist-get result :end-pos)
            (not (= (plist-get result :start-pos)
                   (plist-get result :end-pos))))
        (goto-char (plist-get result :start-pos))
        (push-mark (plist-get result :end-pos) t t)
        (recenter)
        (if (plist-get result :exact-match)
            (message "Region selected in PR diff (lines %d-%d). Adding comment..."
                    start-line end-line)
          (message "Region selected in PR diff (lines %d-%d, closest to %d-%d). Adding comment..."
                  start-line end-line
                  (plist-get result :closest-start)
                  (plist-get result :closest-end)))
        (pr-review-context-comment))

       ;; Single line
       (t
        (goto-char (plist-get result :start-pos))
        (recenter)
        (if (plist-get result :exact-match)
            (message "Positioned at line %d in PR diff. Adding comment..."
                    start-line)
          (message "Positioned at closest line %d in PR diff (requested %d). Adding comment..."
                  (plist-get result :closest-start) start-line))
        (pr-review-context-comment))))))

;; Helper function to open pr-review from magit with forge integration
(defun my/pr-review-from-forge ()
  "Open pr-review for the PR at point in magit/forge.
This captures the git directory automatically."
  (interactive)
  (unless (fboundp 'forge--browse-target)
    (user-error "Forge is not installed or loaded"))

  (if-let* ((target (forge--browse-target))
            (url (if (stringp target) target (forge-get-url target)))
            (rev-url (pr-review-url-parse url)))
      (pr-review url)
    (user-error "No PR to review at point")))


(provide 'pr-review-mods)
;;; pr-review-mods.el ends here
