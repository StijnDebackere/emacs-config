;;; agent-shell-annotations-tests.el --- Annotation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run bin/test-agent-shell-annotations.sh.  Session selection is stubbed;
;; the integration test uses the real agent-shell draft insertion function.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-annotations)

(defmacro sdb/agent-shell-annotations-tests--isolated (&rest body)
  "Run BODY with a private annotation collection and preview buffer."
  (declare (indent 0) (debug t))
  `(let ((sdb/agent-shell-annotations nil)
         (sdb/agent-shell-annotations--next-id 0)
         (sdb/agent-shell-annotations--preview-buffer-name
          (generate-new-buffer-name " *annotation-test-preview*")))
     (unwind-protect
         (progn ,@body)
       (sdb/agent-shell-clear-annotations)
       (when-let* ((preview (get-buffer sdb/agent-shell-annotations--preview-buffer-name)))
         (kill-buffer preview)))))

(defmacro sdb/agent-shell-annotations-tests--with-shell (&rest body)
  "Run BODY with a fake session in SHELL, without launching an agent."
  (declare (indent 0) (debug t))
  `(let ((shell (generate-new-buffer " *annotation-test-shell*"))
         (agent-shell-prefer-viewport-interaction nil))
     (unwind-protect
         (progn
           (with-current-buffer shell
             (setq major-mode 'agent-shell-mode)
             (setq-local agent-shell--state (agent-shell--make-state))
             (map-put! agent-shell--state :session '((:id . "test-session"))))
           ,@body)
       (kill-buffer shell))))

(ert-deftest sdb/agent-shell-annotations-whole-lines-and-boundary ()
  "Expand partial selections, excluding a line whose start ends the region."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "one\n  two\nthree\n")
      (let ((annotation (sdb/agent-shell-annotate-region 6 11 "Explain this")))
        (should (equal "  two\n" (map-elt annotation :text)))
        (should (= 2 (map-elt annotation :first-line)))
        (should (= 2 (map-elt annotation :last-line)))
        (should (= 5 (overlay-start (map-elt annotation :overlay))))
        (should (= 11 (overlay-end (map-elt annotation :overlay))))
        (should (eq 'sdb/agent-shell-annotation-face
                    (overlay-get (map-elt annotation :overlay) 'face)))))))

(ert-deftest sdb/agent-shell-annotations-reversed-region-and-final-line ()
  "Handle reversed selections and the final line without a newline."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "first\nlast")
      (let ((annotation (sdb/agent-shell-annotate-region (point-max) 8 "Last line")))
        (should (equal "last" (map-elt annotation :text)))
        (should (= 2 (map-elt annotation :last-line)))
        (should (= (point-max) (overlay-end (map-elt annotation :overlay))))))))

(ert-deftest sdb/agent-shell-annotations-narrowed-buffer ()
  "Capture absolute line numbers and restore the user's narrowing."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "one\ntwo\nthree\nfour\n")
      (narrow-to-region 9 20)
      (sdb/agent-shell-annotate-region 15 20 "Fourth line")
      (should (= 9 (point-min)))
      (should (= 20 (point-max)))
      (should (= 4 (map-elt (car sdb/agent-shell-annotations) :first-line))))))

(ert-deftest sdb/agent-shell-annotations-reject-invalid-input ()
  "Invalid selections and blank comments leave no pending notes."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "text")
      (dolist (range '((1 1) (0 2) (1 99)))
        (should-error (sdb/agent-shell-annotate-region (car range) (cadr range) "note")
                      :type 'user-error))
      (should-error (sdb/agent-shell-annotate-region 1 3 " \n\t") :type 'user-error)
      (should-not sdb/agent-shell-annotations)
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest sdb/agent-shell-annotations-interactive-selection-and-cancel ()
  "Require an active selection and preserve state when commenting is cancelled."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "text")
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
        (should-error (call-interactively #'sdb/agent-shell-annotate-region)
                      :type 'user-error))
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () 3))
                ((symbol-function 'read-string) (lambda (&rest _) (signal 'quit nil))))
        (condition-case nil
            (progn (call-interactively #'sdb/agent-shell-annotate-region)
                   (ert-fail "Comment prompt should be cancelled"))
          (quit nil)))
      (should-not sdb/agent-shell-annotations))))

(ert-deftest sdb/agent-shell-annotations-multiple-buffers-and-unsaved-text ()
  "Collect snapshots from files and non-file buffers in selection order."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (setq buffer-file-name "/tmp/first.py")
      (insert (propertize "unsaved = 1\n" 'face 'bold))
      (sdb/agent-shell-annotate-region 1 (point-max) "First comment"))
    (with-temp-buffer
      (insert "scratch text")
      (sdb/agent-shell-annotate-region 1 (point-max) "Second comment"))
    (let ((batch (sdb/agent-shell-annotations--format-batch)))
      (should (= 2 (length sdb/agent-shell-annotations)))
      (should (< (string-match "First comment" batch) (string-match "Second comment" batch)))
      (should (string-match-p "Source: \"/tmp/first.py\"" batch))
      (should (string-match-p "Source: \"Buffer: " batch))
      (should (string-match-p "1: unsaved = 1" batch))
      (should-not (get-text-property 0 'face
                                     (map-elt (car sdb/agent-shell-annotations) :text))))))

(ert-deftest sdb/agent-shell-annotations-preserve-snapshot-after-source-edits ()
  "Overlay locations move while the captured text and line numbers stay fixed."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "first\nsecond\n")
      (let* ((annotation (sdb/agent-shell-annotate-region 7 (point-max) "Captured second"))
             (overlay (map-elt annotation :overlay)))
        (goto-char (point-min))
        (insert "new\n")
        (should (= 11 (overlay-start overlay)))
        (delete-region (overlay-start overlay) (overlay-end overlay))
        (should (equal "second\n" (map-elt annotation :text)))
        (should (= 2 (map-elt annotation :first-line)))
        (should (string-match-p "2: second" (sdb/agent-shell-annotations--format-batch)))))))

(ert-deftest sdb/agent-shell-annotations-closed-source-and-edit ()
  "Closing a source buffer retains the snapshot and allows comment editing."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "original")
      (sdb/agent-shell-annotate-region 1 (point-max) "Old comment"))
    (let ((annotation (car sdb/agent-shell-annotations)))
      (should-not (buffer-live-p (map-elt annotation :buffer)))
      (sdb/agent-shell-edit-annotation annotation "New comment\nMore detail")
      (should (string-match-p "New comment\nMore detail" (sdb/agent-shell-annotations--format-batch)))
      (should (string-match-p "1: original" (sdb/agent-shell-annotations--format-batch))))))

(ert-deftest sdb/agent-shell-annotations-format-blank-lines-and-fences ()
  "Preserve blank lines and safely quote source containing Markdown fences."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "first\n\n```\n````\n")
      (sdb/agent-shell-annotate-region 1 (point-max) "Literal fences"))
    (let ((batch (sdb/agent-shell-annotations--format-batch)))
      (should (string-match-p "`````text\n1: first\n2: \n3: ```\n4: ````\n`````" batch))
      (should-not (string-match-p "5: " batch)))))

(ert-deftest sdb/agent-shell-annotations-point-selection-and-overlap ()
  "Select the highlighted note at point and disambiguate overlapping notes."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "one\ntwo\n")
      (let ((first (sdb/agent-shell-annotate-region 1 5 "First")))
        (goto-char 2)
        (should (eq first (sdb/agent-shell-annotations--read-annotation)))
        (sdb/agent-shell-annotate-region 1 (point-max) "Overlap")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _) (caar choices))))
          (should (eq first (sdb/agent-shell-annotations--read-annotation))))))))

(ert-deftest sdb/agent-shell-annotations-preview-edit-remove-and-clear ()
  "Preview edits update comments, while removal and clearing clean overlays."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "one\ntwo\n")
      (let* ((first (sdb/agent-shell-annotate-region 1 5 "Old"))
             (second (sdb/agent-shell-annotate-region 5 (point-max) "Keep"))
             (first-overlay (map-elt first :overlay))
             (second-overlay (map-elt second :overlay)))
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
          (sdb/agent-shell-preview-annotations))
        (with-current-buffer sdb/agent-shell-annotations--preview-buffer-name
          (should buffer-read-only)
          (goto-char (point-min))
          (search-forward "Annotation 1")
          (should (eq first (sdb/agent-shell-annotations--read-annotation)))
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Revised")))
            (call-interactively #'sdb/agent-shell-edit-annotation))
          (should (string-match-p "Revised" (buffer-string)))
          (should-not (string-match-p "Comment:\nOld" (buffer-string)))
          (should (eq (key-binding (kbd "C-c C-c")) #'sdb/agent-shell-send-annotations-to))
          (call-interactively #'sdb/agent-shell-remove-annotation))
        (should-not (overlay-buffer first-overlay))
        (should (equal (list second) sdb/agent-shell-annotations))
        (should (equal "Note 1: Revised" (overlay-get first-overlay 'help-echo)))
        (sdb/agent-shell-clear-annotations)
        (should-not (overlay-buffer second-overlay))
        (with-current-buffer sdb/agent-shell-annotations--preview-buffer-name
          (should (equal "No annotations pending.\n" (buffer-string))))))))

(ert-deftest sdb/agent-shell-annotations-reject-stale-or-blank-edits ()
  "Failed edits preserve valid comments and reject removed annotations."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "one")
      (let ((annotation (sdb/agent-shell-annotate-region 1 (point-max) "Keep")))
        (should-error (sdb/agent-shell-edit-annotation annotation " ") :type 'user-error)
        (should (equal "Keep" (map-elt annotation :comment)))
        (sdb/agent-shell-remove-annotation annotation)
        (should-error (sdb/agent-shell-edit-annotation annotation "Lost") :type 'user-error)
        (should-error (sdb/agent-shell-remove-annotation annotation) :type 'user-error)))))

(ert-deftest sdb/agent-shell-annotations-empty-batch ()
  "Empty collections cannot be previewed, selected, or sent."
  (sdb/agent-shell-annotations-tests--isolated
    (should-error (sdb/agent-shell-preview-annotations) :type 'user-error)
    (should-error (sdb/agent-shell-annotations--read-annotation) :type 'user-error)
    (cl-letf (((symbol-function 'agent-shell--read-shell-buffer)
               (lambda (&rest _) (ert-fail "No session should be selected"))))
      (should-error (sdb/agent-shell-send-annotations-to) :type 'user-error))))

(ert-deftest sdb/agent-shell-annotations-draft-insertion-integration ()
  "Use real draft insertion once, retaining existing input and never submitting."
  (sdb/agent-shell-annotations-tests--isolated
    (sdb/agent-shell-annotations-tests--with-shell
      (with-current-buffer shell (insert "Agent> existing draft"))
      (with-temp-buffer
        (insert "one\ntwo\n")
        (sdb/agent-shell-annotate-region 1 5 "First")
        (sdb/agent-shell-annotate-region 5 (point-max) "Second")
        (let ((expected (substring-no-properties (sdb/agent-shell-annotations--format-batch)))
              (source (buffer-string)))
          (cl-letf (((symbol-function 'agent-shell--read-shell-buffer) (lambda (&rest _) shell))
                    ((symbol-function 'agent-shell--display-buffer) (lambda (buffer) (set-buffer buffer)))
                    ((symbol-function 'shell-maker-busy) (lambda () nil))
                    ((symbol-function 'shell-maker-submit)
                     (lambda (&rest _) (ert-fail "Draft must not be submitted"))))
            (save-current-buffer (sdb/agent-shell-send-annotations-to)))
          (should-not sdb/agent-shell-annotations)
          (should (equal source (buffer-string)))
          (should-not (overlays-in (point-min) (point-max)))
          (with-current-buffer shell
            (should (equal (concat "Agent> existing draft\n\n" expected) (buffer-string)))
            (should-not (text-property-any (point-min) (point-max)
                                          'sdb/agent-shell-annotation-id 1))))))))

(ert-deftest sdb/agent-shell-annotations-session-picker-cancel-retains-batch ()
  "Cancelling the session picker leaves snapshots and highlights pending."
  (sdb/agent-shell-annotations-tests--isolated
    (with-temp-buffer
      (insert "one")
      (let ((annotation (sdb/agent-shell-annotate-region 1 (point-max) "Keep")))
        (cl-letf (((symbol-function 'agent-shell--read-shell-buffer)
                   (lambda (&rest _) (signal 'quit nil)))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest _) (ert-fail "Must not insert after cancellation"))))
          (condition-case nil
              (progn (sdb/agent-shell-send-annotations-to) (ert-fail "Picker should quit"))
            (quit nil)))
        (should (equal (list annotation) sdb/agent-shell-annotations))
        (should (overlay-buffer (map-elt annotation :overlay)))))))

(ert-deftest sdb/agent-shell-annotations-busy-session-retains-batch ()
  "A busy session without a prompt does not consume or insert the batch."
  (sdb/agent-shell-annotations-tests--isolated
    (sdb/agent-shell-annotations-tests--with-shell
      (with-temp-buffer
        (insert "one")
        (sdb/agent-shell-annotate-region 1 (point-max) "Keep")
        (cl-letf (((symbol-function 'agent-shell--read-shell-buffer) (lambda (&rest _) shell))
                  ((symbol-function 'shell-maker-busy) (lambda () t))
                  ((symbol-function 'agent-shell--prompt-input-start) (lambda () nil))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest _) (ert-fail "Busy session should not receive text"))))
          (should-error (sdb/agent-shell-send-annotations-to) :type 'user-error))
        (should (= 1 (length sdb/agent-shell-annotations)))))))

(ert-deftest sdb/agent-shell-annotations-unready-session-retains-batch ()
  "An existing shell without a session cannot defer or consume the batch."
  (sdb/agent-shell-annotations-tests--isolated
    (sdb/agent-shell-annotations-tests--with-shell
      (with-current-buffer shell (map-put! agent-shell--state :session nil))
      (with-temp-buffer
        (insert "one")
        (sdb/agent-shell-annotate-region 1 (point-max) "Keep")
        (cl-letf (((symbol-function 'agent-shell--read-shell-buffer) (lambda (&rest _) shell))
                  ((symbol-function 'agent-shell-insert)
                   (lambda (&rest _) (ert-fail "Unready session should not receive text"))))
          (should-error (sdb/agent-shell-send-annotations-to) :type 'user-error))
        (should (= 1 (length sdb/agent-shell-annotations)))))))

(ert-deftest sdb/agent-shell-annotations-busy-live-prompt-integration ()
  "A persistent prompt accepts a draft while the agent is busy."
  (sdb/agent-shell-annotations-tests--isolated
    (sdb/agent-shell-annotations-tests--with-shell
      (with-temp-buffer
        (insert "one")
        (sdb/agent-shell-annotate-region 1 (point-max) "Check this")
        (let ((expected (substring-no-properties (sdb/agent-shell-annotations--format-batch))))
          (cl-letf (((symbol-function 'agent-shell--read-shell-buffer) (lambda (&rest _) shell))
                    ((symbol-function 'agent-shell--display-buffer) (lambda (buffer) (set-buffer buffer)))
                    ((symbol-function 'shell-maker-busy) (lambda () t))
                    ((symbol-function 'agent-shell--prompt-input-start) (lambda () 1))
                    ((symbol-function 'shell-maker-submit)
                     (lambda (&rest _) (ert-fail "Busy prompt must not submit"))))
            (save-current-buffer (sdb/agent-shell-send-annotations-to)))
          (should-not sdb/agent-shell-annotations)
          (with-current-buffer shell
            (should (equal (concat "\n\n" expected) (buffer-string)))))))))

(ert-deftest sdb/agent-shell-annotations-viewport-draft-routing ()
  "Respect viewport preference and pass the batch without submission."
  (sdb/agent-shell-annotations-tests--isolated
    (sdb/agent-shell-annotations-tests--with-shell
      (let ((agent-shell-prefer-viewport-interaction t)
            received)
        (with-temp-buffer
          (insert "one")
          (sdb/agent-shell-annotate-region 1 (point-max) "Check this")
          (let ((expected (substring-no-properties (sdb/agent-shell-annotations--format-batch))))
            (cl-letf (((symbol-function 'agent-shell--read-shell-buffer) (lambda (&rest _) shell))
                      ((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-viewport--show-buffer)
                       (lambda (&rest args)
                         (setq received args)
                         (list (cons :buffer shell))))
                      ((symbol-function 'agent-shell--insert-to-shell-buffer)
                       (lambda (&rest _) (ert-fail "Viewport preference should be honored"))))
              (sdb/agent-shell-send-annotations-to))
            (should (equal expected (plist-get received :append)))
            (should (eq shell (plist-get received :shell-buffer)))
            (should (plist-member received :submit))
            (should-not (plist-get received :submit))
            (should-not sdb/agent-shell-annotations)))))))

(ert-deftest sdb/agent-shell-annotations-insertion-errors-retain-batch ()
  "Failed insertion preserves notes for retry, both for errors and nil results."
  (sdb/agent-shell-annotations-tests--isolated
    (sdb/agent-shell-annotations-tests--with-shell
      (with-temp-buffer
        (insert "one")
        (let ((annotation (sdb/agent-shell-annotate-region 1 (point-max) "Keep")))
          (dolist (failure (list (lambda (&rest _) (error "Insertion failed"))
                                (lambda (&rest _) nil)))
            (cl-letf (((symbol-function 'agent-shell--read-shell-buffer) (lambda (&rest _) shell))
                      ((symbol-function 'shell-maker-busy) (lambda () nil))
                      ((symbol-function 'agent-shell-insert) failure))
              (should-error (sdb/agent-shell-send-annotations-to)))
            (should (equal (list annotation) sdb/agent-shell-annotations))
            (should (overlay-buffer (map-elt annotation :overlay)))))))))

(provide 'agent-shell-annotations-tests)
;;; agent-shell-annotations-tests.el ends here
