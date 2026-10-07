;;; agent-shell-pairing-tests.el --- Prompt pairing tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-pairing)

(defmacro sdb/agent-shell-pairing-tests--isolated (&rest body)
  "Run BODY with native insertion hooks and isolated pairing configuration."
  (declare (indent 0) (debug t))
  `(let ((electric-pair-mode t)
         (electric-pair-preserve-balance t)
         (electric-pair-pairs (copy-tree (default-value 'electric-pair-pairs)))
         (electric-pair-text-pairs (copy-tree (default-value 'electric-pair-text-pairs)))
         (post-self-insert-hook '(electric-pair-post-self-insert-function))
         (agent-shell-mode-hook nil)
         (agent-shell-viewport-edit-mode-hook nil)
         (transient-mark-mode t))
     (unwind-protect
         (progn (sdb/agent-shell-pairing-enable) ,@body)
       (advice-remove 'electric-pair-post-self-insert-function
                      #'sdb/agent-shell-pairing--insert))))

(defmacro sdb/agent-shell-pairing-tests--prompt (history &rest body)
  "Run BODY in a mock shell prompt after arbitrary HISTORY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (set-syntax-table agent-shell-mode-syntax-table)
     (insert (propertize ,history 'field 'output 'read-only t 'rear-nonsticky t))
     (let ((prompt-beg (point)))
       (insert (propertize "Agent> " 'field 'prompt 'read-only t 'rear-nonsticky t))
       (setq-local comint-last-prompt (cons (copy-marker prompt-beg) (copy-marker (point))))
       (let ((input-beg (point)))
         (sdb/agent-shell-pairing-setup)
         ,@body))))

(defun sdb/agent-shell-pairing-tests--type (text)
  "Type TEXT through the native self-insert command and its real hooks."
  (dolist (char (string-to-list text))
    (let ((last-command-event char)) (self-insert-command 1))))

(ert-deftest sdb/agent-shell-pairing-all-delimiters-ignore-transcript ()
  "Openers pair and typed closers skip, despite unmatched transcript syntax."
  (sdb/agent-shell-pairing-tests--isolated
    (dolist (history '("Unmatched ((((\n" "Unmatched \"\n" "((\"[{'`\n"))
      (dolist (pair '("()" "[]" "{}" "\"\"" "``" "''"))
        (sdb/agent-shell-pairing-tests--prompt history
          (sdb/agent-shell-pairing-tests--type (substring pair 0 1))
          (should (equal pair (buffer-substring-no-properties input-beg (point-max))))
          (should (= (1+ input-beg) (point)))
          (sdb/agent-shell-pairing-tests--type (substring pair 1))
          (should (equal pair (buffer-substring-no-properties input-beg (point-max))))
          (should (= (+ input-beg 2) (point)))
          (should (equal (concat history "Agent> ")
                         (buffer-substring-no-properties (point-min) input-beg))))))))

(ert-deftest sdb/agent-shell-pairing-quoted-text-and-contractions ()
  "Quoted words close once; contractions and possessives retain literal quotes."
  (sdb/agent-shell-pairing-tests--isolated
    (dolist (text '("\"hello\"" "'hello'" "`hello`" "don't" "users'" "it's correct" "'don't'"))
      (sdb/agent-shell-pairing-tests--prompt "Earlier \"((((\n"
        (sdb/agent-shell-pairing-tests--type text)
        (should (equal text (buffer-substring-no-properties input-beg (point-max))))
        (should (= (point) (point-max)))))))

(ert-deftest sdb/agent-shell-pairing-apostrophe-inside-existing-word ()
  "Inserting an apostrophe inside an existing word does not add a second one."
  (sdb/agent-shell-pairing-tests--isolated
    (sdb/agent-shell-pairing-tests--prompt "\"((\n"
      (insert "dont")
      (backward-char)
      (sdb/agent-shell-pairing-tests--type "'")
      (should (equal "don't" (buffer-substring-no-properties input-beg (point-max)))))))

(ert-deftest sdb/agent-shell-pairing-region-wrapping ()
  "Region wrapping works from either end, including quotes after a word."
  (sdb/agent-shell-pairing-tests--isolated
    (dolist (pair '("()" "[]" "{}" "\"\"" "``" "''"))
      (dolist (backwards '(nil t))
        (sdb/agent-shell-pairing-tests--prompt "Bad history \"((((\n"
          (insert "hello")
          (if backwards
              (progn (set-mark input-beg) (goto-char (point-max)))
            (set-mark (point-max)) (goto-char input-beg))
          (setq mark-active t)
          (sdb/agent-shell-pairing-tests--type (substring pair 0 1))
          (should (equal (concat (substring pair 0 1) "hello" (substring pair 1))
                         (buffer-substring-no-properties input-beg (point-max)))))))))

(ert-deftest sdb/agent-shell-pairing-escaped-delimiters ()
  "Backslash-escaped openers stay literal through the native hook."
  (sdb/agent-shell-pairing-tests--isolated
    (dolist (text '("\\(" "\\\"" "\\`" "\\'"))
      (sdb/agent-shell-pairing-tests--prompt "Bad history \"((((\n"
        (sdb/agent-shell-pairing-tests--type text)
        (should (equal text (buffer-substring-no-properties input-beg (point-max))))))))

(ert-deftest sdb/agent-shell-pairing-viewport-composer ()
  "The viewport composer gets the same quote pairs and word handling."
  (sdb/agent-shell-pairing-tests--isolated
    (dolist (text '("()" "\"\"" "[]" "{}" "``" "''" "don't"))
      (with-temp-buffer
        (setq major-mode 'agent-shell-viewport-edit-mode)
        (set-syntax-table text-mode-syntax-table)
        (run-hooks 'agent-shell-viewport-edit-mode-hook)
        (sdb/agent-shell-pairing-tests--type text)
        (should (equal text (buffer-string)))))))

(ert-deftest sdb/agent-shell-pairing-preserves-narrowing ()
  "Temporary prompt isolation restores the caller's existing restriction."
  (sdb/agent-shell-pairing-tests--isolated
    (sdb/agent-shell-pairing-tests--prompt "Bad history \"((((\n"
      (narrow-to-region 2 (point-max))
      (sdb/agent-shell-pairing-tests--type "()")
      (should (= 2 (point-min)))
      (should (equal "()" (buffer-substring-no-properties input-beg (point-max)))))))

(ert-deftest sdb/agent-shell-pairing-no-live-prompt ()
  "An absent or stale live prompt does not cause pairing in the transcript."
  (sdb/agent-shell-pairing-tests--isolated
    (with-temp-buffer
      (setq major-mode 'agent-shell-mode)
      (sdb/agent-shell-pairing-setup)
      (sdb/agent-shell-pairing-tests--type "(")
      (should (equal "(" (buffer-string))))
    (sdb/agent-shell-pairing-tests--prompt "History\n"
      (insert (propertize "Output\n" 'field 'output))
      (sdb/agent-shell-pairing-tests--type "(")
      (should (equal "Output\n(" (buffer-substring-no-properties input-beg (point-max)))))))

(ert-deftest sdb/agent-shell-pairing-setup-is-local-and-idempotent ()
  "Setup preserves global settings, custom pairs, and a single advice wrapper."
  (let ((defaults (copy-tree (default-value 'electric-pair-pairs))))
    (sdb/agent-shell-pairing-tests--isolated
      (sdb/agent-shell-pairing-enable)
      (sdb/agent-shell-pairing-tests--prompt "History\n"
        (should (= input-beg (sdb/agent-shell-pairing--input-start)))
        (push '(?< . ?>) electric-pair-pairs)
        (run-hooks 'agent-shell-mode-hook)
        (sdb/agent-shell-pairing-setup)
        (should (local-variable-p 'electric-pair-pairs))
        (should (local-variable-p 'electric-pair-text-pairs))
        (should (equal '(?< . ?>) (assq ?< electric-pair-pairs)))
        (dolist (char '(?` ?\'))
          (should (= 1 (seq-count (lambda (pair) (eq char (car pair))) electric-pair-pairs)))))
      (should (equal defaults (default-value 'electric-pair-pairs)))
      (with-temp-buffer
        (sdb/agent-shell-pairing-setup)
        (should-not (local-variable-p 'electric-pair-pairs))
        (sdb/agent-shell-pairing-tests--type "(")
        (should (equal "()" (buffer-string)))))))

(provide 'agent-shell-pairing-tests)
;;; agent-shell-pairing-tests.el ends here
