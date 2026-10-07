;;; agent-shell-pairing.el --- Pair delimiters within agent prompts -*- lexical-binding: t; -*-

;;; Commentary:
;; Electric Pair's balance checks must not parse the conversation transcript.
;; Narrow its insertion hook to the live shell prompt (or viewport composer).
;; Add Markdown backticks and single quotes, keeping word apostrophes literal.
;; Run bin/test-agent-shell-pairing.sh for the isolated insertion tests.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-prompt)
(require 'agent-shell-viewport)
(require 'elec-pair)
(require 'seq)

(defun sdb/agent-shell-pairing--input-start ()
  "Return the editable input's start in the current supported buffer."
  (cond
   ((derived-mode-p 'agent-shell-mode)
    (agent-shell--prompt-input-start))
   ((derived-mode-p 'agent-shell-viewport-edit-mode)
    (point-min))))

(defun sdb/agent-shell-pairing--word-apostrophe-p ()
  "Return non-nil if the just-typed apostrophe belongs to a word.
A quote immediately ahead may be an automatic closer; let Electric Pair
skip that closer when the user explicitly types it."
  (and (eq last-command-event ?\')
       (not (use-region-p))
       (> (point) (1+ (point-min)))
       (memq (char-syntax (char-before (1- (point)))) '(?w ?_))
       (not (eq (char-after) ?\'))))

(defun sdb/agent-shell-pairing--insert (original &rest args)
  "Run ORIGINAL with ARGS, isolating pairing to the editable agent prompt."
  (if (not (derived-mode-p 'agent-shell-mode 'agent-shell-viewport-edit-mode))
      (apply original args)
    (when-let* ((start (sdb/agent-shell-pairing--input-start))
                ;; The newly inserted character must also be inside the prompt.
                ((> (point) start))
                ((or (not (use-region-p)) (>= (region-beginning) start))))
      (save-restriction
        (narrow-to-region start (point-max))
        (unless (sdb/agent-shell-pairing--word-apostrophe-p)
          (apply original args))))))

(defun sdb/agent-shell-pairing-setup ()
  "Add prompt-specific quote pairs in the current buffer, without duplicates."
  (when (derived-mode-p 'agent-shell-mode 'agent-shell-viewport-edit-mode)
    (dolist (variable '(electric-pair-pairs electric-pair-text-pairs))
      (set (make-local-variable variable)
           (append '((?` . ?`) (?\' . ?\'))
                   (seq-remove (lambda (pair) (memq (car pair) '(?` ?\')))
                               (symbol-value variable)))))))

(defun sdb/agent-shell-pairing-enable ()
  "Enable prompt-scoped Electric Pair in existing and future agent buffers."
  (advice-add 'electric-pair-post-self-insert-function :around
              #'sdb/agent-shell-pairing--insert)
  (add-hook 'agent-shell-mode-hook #'sdb/agent-shell-pairing-setup)
  (add-hook 'agent-shell-viewport-edit-mode-hook #'sdb/agent-shell-pairing-setup)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer (sdb/agent-shell-pairing-setup))))

(provide 'agent-shell-pairing)
;;; agent-shell-pairing.el ends here
