;;; test-agent-shell-command-output.el --- Local package checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with Emacs --batch -Q -l this file after Straight builds dependencies.

;;; Code:

(let* ((config-dir (expand-file-name ".." (file-name-directory load-file-name)))
       (repo (expand-file-name "local/agent-shell" config-dir)))
  (setq load-prefer-newer t)
  (dolist (dir (directory-files (expand-file-name "straight/build" config-dir)
                               t "^[^.]" t))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir)))
  (add-to-list 'load-path repo)
  (require 'agent-shell)
  (dolist (file '("tests/agent-shell-tests.el"
                  "tests/agent-shell-ui-tests.el"
                  "tests/agent-shell-command-output-tests.el"))
    (load (expand-file-name file repo) nil t))
  (ert-run-tests-batch-and-exit
   "tool.*output\\|activity-group\\|agent-shell-ui-\\|agent-shell-command-output-\\|agent-shell-next-item-\\|agent-shell-previous-item-"))

;;; test-agent-shell-command-output.el ends here
