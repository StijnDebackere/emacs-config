;;; agent-shell-magit-links-tests.el --- Magit review link tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Test restricted URL routing and actual staged diffs in a temporary repository.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-diff)
(require 'agent-shell-magit-links)

(defmacro sdb/agent-shell-magit-links-test--fixture (&rest body)
  "Run BODY in a temporary local directory with Magit link advice installed."
  (declare (indent 0) (debug t))
  `(let* ((original-buffers (buffer-list))
          (directory (make-temp-file "sdb-magit-links-" t))
          (default-directory (file-name-as-directory directory)))
     (unwind-protect
         (progn
           (sdb/agent-shell-magit-links-enable)
           ,@body)
       (advice-remove 'agent-shell-markdown--open-link
                      #'sdb/agent-shell-magit-links--open)
       (advice-remove 'agent-shell-markdown--link-verb
                      #'sdb/agent-shell-magit-links--verb)
       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
           (when (and (not (memq buffer original-buffers))
                      (string-prefix-p (file-name-as-directory directory) default-directory))
             (set-buffer-modified-p nil)
             (kill-buffer buffer))))
       (delete-directory directory t))))

(ert-deftest sdb/agent-shell-magit-links-current-repo ()
  (sdb/agent-shell-magit-links-test--fixture
    (let (call)
      (cl-letf (((symbol-function 'magit-toplevel)
                 (lambda (dir) (should (equal dir default-directory)) dir))
                ((symbol-function 'magit-diff-staged)
                 (lambda (&rest args)
                   (setq call (list default-directory args)))))
        (agent-shell-markdown--open-link "magit:staged"))
      (should (equal call (list default-directory '(nil nil nil)))))))

(ert-deftest sdb/agent-shell-magit-links-explicit-repo-with-spaces ()
  (sdb/agent-shell-magit-links-test--fixture
    (let* ((target (expand-file-name "repo with spaces" directory))
           (url (concat "magit:staged?repo=" (url-hexify-string target)))
           call)
      (make-directory target)
      (cl-letf (((symbol-function 'magit-toplevel)
                 (lambda (dir)
                   (should (equal dir (file-name-as-directory target))) dir))
                ((symbol-function 'magit-diff-staged)
                 (lambda (&rest args) (setq call (list default-directory args)))))
        (agent-shell-markdown--open-link url))
      (should (equal call (list (file-name-as-directory target) '(nil nil nil)))))))

(ert-deftest sdb/agent-shell-magit-links-rejects-actions-and-queries ()
  (sdb/agent-shell-magit-links-test--fixture
    (cl-letf (((symbol-function 'browse-url)
               (lambda (&rest _) (ert-fail "Unsupported Magit link reached browser")))
              ((symbol-function 'magit-diff-staged)
               (lambda (&rest _) (ert-fail "Unsupported action opened Magit"))))
      (dolist (url '("magit:commit" "magit:stage" "magit:eval=(message)"
                     "magit:staged?eval=anything" "magit:staged?repo="
                     "magit:staged?repo=/tmp&action=commit"
                     "magit:staged#anything"))
        (should-error (agent-shell-markdown--open-link url) :type 'user-error)))))

(ert-deftest sdb/agent-shell-magit-links-rejects-invalid-paths ()
  (sdb/agent-shell-magit-links-test--fixture
    (dolist (suffix '("relative" "%GG" "%2" "%00" "%0A" "/does-not-exist-sdb-test"))
      (should-error
       (agent-shell-markdown--open-link (concat "magit:staged?repo=" suffix))
       :type 'user-error))))

(ert-deftest sdb/agent-shell-magit-links-rejects-remote-without-connection ()
  (sdb/agent-shell-magit-links-test--fixture
    (let ((original (symbol-function 'file-directory-p)))
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (path)
                   ;; Loading TRAMP may check local library directories.
                   (if (string-prefix-p "/ssh:" path)
                       (ert-fail "Remote filesystem was accessed")
                     (funcall original path)))))
        (should-error
         (agent-shell-markdown--open-link "magit:staged?repo=/ssh:example.invalid:/tmp/")
         :type 'user-error)))))

(ert-deftest sdb/agent-shell-magit-links-rejects-non-git-directory ()
  (sdb/agent-shell-magit-links-test--fixture
    (should-error (agent-shell-markdown--open-link "magit:staged") :type 'user-error)))

(ert-deftest sdb/agent-shell-magit-links-other-urls-unchanged ()
  (sdb/agent-shell-magit-links-test--fixture
    (let (opened)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq opened url))))
        (agent-shell-markdown--open-link "https://example.invalid/plot.html"))
      (should (equal opened "https://example.invalid/plot.html")))))

(ert-deftest sdb/agent-shell-magit-links-link-hints ()
  (sdb/agent-shell-magit-links-test--fixture
    (should (equal (agent-shell-markdown--link-verb "magit:staged" nil)
                   "review staged changes in Magit"))
    (should (equal (agent-shell-markdown--link-verb "magit:staged" "custom action")
                   "custom action"))
    (should (equal (agent-shell-markdown--link-verb "https://example.invalid/" nil)
                   "open in browser"))))

(ert-deftest sdb/agent-shell-magit-links-rendered-ret-action ()
  (sdb/agent-shell-magit-links-test--fixture
    (let (opened)
      (cl-letf (((symbol-function 'magit-toplevel) (lambda (dir) dir))
                ((symbol-function 'magit-diff-staged)
                 (lambda (&rest args) (setq opened args))))
        (with-temp-buffer
          (insert "Review staged changes")
          (agent-shell-markdown--apply-link-properties
           :start (point-min) :end (point-max) :url "magit:staged")
          (let* ((map (get-text-property (point-min) 'keymap))
                 (action (lookup-key map (kbd "RET")))
                 (help (get-text-property (point-min) 'help-echo)))
            (should (commandp action))
            (should (equal (funcall help nil nil (point-min))
                           "Review staged changes in Magit"))
            (call-interactively action))))
      (should (equal opened '(nil nil nil))))))

(ert-deftest sdb/agent-shell-magit-links-install-idempotent ()
  (sdb/agent-shell-magit-links-test--fixture
    (sdb/agent-shell-magit-links-enable)
    (sdb/agent-shell-magit-links-enable)
    (dolist (pair '((agent-shell-markdown--open-link . sdb/agent-shell-magit-links--open)
                    (agent-shell-markdown--link-verb . sdb/agent-shell-magit-links--verb)))
      (let ((count 0))
        (advice-mapc (lambda (function _)
                       (when (eq function (cdr pair)) (cl-incf count)))
                     (car pair))
        (should (= count 1))))))

(defun sdb/agent-shell-magit-links-test--git (&rest args)
  "Run local Git with ARGS, failing the test if it exits unsuccessfully."
  (with-temp-buffer
    (should (= 0 (apply #'process-file "git" nil (current-buffer) nil args)))
    (buffer-string)))

(ert-deftest sdb/agent-shell-magit-links-real-staged-diff-preserves-index ()
  (sdb/agent-shell-magit-links-test--fixture
    (sdb/agent-shell-magit-links-test--git "init" "--quiet")
    (with-temp-file "sample.txt" (insert "original\n"))
    (sdb/agent-shell-magit-links-test--git "add" "sample.txt")
    (sdb/agent-shell-magit-links-test--git
     "-c" "user.name=ERT" "-c" "user.email=ert@example.invalid"
     "-c" "commit.gpgsign=false" "-c" "core.hooksPath=/dev/null"
     "commit" "--quiet" "-m" "Fixture")
    (with-temp-file "sample.txt" (insert "staged-only-marker\n"))
    (sdb/agent-shell-magit-links-test--git "add" "sample.txt")
    (with-temp-file "sample.txt" (insert "unstaged-only-marker\n"))
    (let ((head (sdb/agent-shell-magit-links-test--git "rev-parse" "HEAD"))
          (staged (sdb/agent-shell-magit-links-test--git "diff" "--cached" "--binary"))
          (unstaged (sdb/agent-shell-magit-links-test--git "diff" "--binary")))
      (save-window-excursion
        (agent-shell-markdown--open-link "magit:staged")
        (should (derived-mode-p 'magit-diff-mode))
        (should (string-match-p "staged-only-marker" (buffer-string)))
        (should-not (string-match-p "unstaged-only-marker" (buffer-string))))
      (should (equal head (sdb/agent-shell-magit-links-test--git "rev-parse" "HEAD")))
      (should (equal staged (sdb/agent-shell-magit-links-test--git "diff" "--cached" "--binary")))
      (should (equal unstaged (sdb/agent-shell-magit-links-test--git "diff" "--binary"))))))

(provide 'agent-shell-magit-links-tests)
;;; agent-shell-magit-links-tests.el ends here
