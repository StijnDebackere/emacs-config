;;; agent-shell-annotations.el --- Collect annotated excerpts -*- lexical-binding: t; -*-

;;; Commentary:
;; Highlight regions across buffers, attach comments, and collect them into
;; one draft for an existing agent-shell session.  Excerpts are snapshots;
;; overlays follow subsequent edits, but the captured text stays unchanged.
;;
;; With the bindings in init.el: select a region, C-c a r, and enter a
;; comment.  Repeat in any buffer.  C-c a p previews the batch; e edits the
;; note at point and d removes it.  C-c a s (or C-c C-c in the preview)
;; selects an existing session and inserts the batch for manual submission.
;; C-c a e/d also edit/remove notes from their highlighted source regions;
;; C-c a c clears the entire batch.  Pending notes live in memory only.
;;
;; Run bin/test-agent-shell-annotations.sh for the isolated ERT suite.

;;; Code:

(require 'agent-shell)
(require 'map)
(require 'seq)
(require 'subr-x)

(defface sdb/agent-shell-annotation-face
  '((t :inherit secondary-selection))
  "Face for a region collected for an agent-shell annotation."
  :group 'agent-shell)

(defvar sdb/agent-shell-annotations nil
  "Pending annotation alists in capture order.
Each has :id, :buffer, :source, :first-line, :last-line, :text,
:comment, and :overlay.  Source and line numbers describe the snapshot.")

(defvar sdb/agent-shell-annotations--next-id 0
  "Last assigned annotation identifier.")

(defvar sdb/agent-shell-annotations--preview-buffer-name
  "*Agent annotations*"
  "Buffer used to preview the pending annotation batch.")

(defun sdb/agent-shell-annotations--require-comment (comment)
  "Return COMMENT, or signal a user error when it is blank."
  (unless (and (stringp comment) (not (string-empty-p (string-trim comment))))
    (user-error "Please enter a non-blank comment"))
  comment)

(defun sdb/agent-shell-annotate-region (beg end comment)
  "Capture lines intersecting BEG through END with COMMENT.
For example, selecting part of line 12 captures all of line 12.
The annotation retains its text and original line numbers after edits."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "Select a region to annotate"))
     (list (region-beginning) (region-end) (read-string "Comment: "))))
  (sdb/agent-shell-annotations--require-comment comment)
  (let ((from (min beg end))
        (to (max beg end)))
    (save-restriction
      (widen)
      (unless (<= (point-min) from (1- to) (1- (point-max)))
        (user-error "Select a non-empty region within the buffer"))
      (save-excursion
        (goto-char from)
        (let ((start (line-beginning-position))
              (first-line (line-number-at-pos)))
          (goto-char (1- to))
          (let* ((last-line (line-number-at-pos))
                 (finish (min (point-max) (1+ (line-end-position))))
                 (id (setq sdb/agent-shell-annotations--next-id
                           (1+ sdb/agent-shell-annotations--next-id)))
                 (overlay (make-overlay start finish nil nil t))
                 (annotation
                  (list (cons :id id)
                        (cons :buffer (current-buffer))
                        (cons :source (or buffer-file-name
                                          (format "Buffer: %s" (buffer-name))))
                        (cons :first-line first-line)
                        (cons :last-line last-line)
                        (cons :text (buffer-substring-no-properties start finish))
                        (cons :comment comment)
                        (cons :overlay overlay))))
            (overlay-put overlay 'sdb/agent-shell-annotation-id id)
            (overlay-put overlay 'face 'sdb/agent-shell-annotation-face)
            (overlay-put overlay 'help-echo (format "Note %d: %s" id comment))
            (overlay-put overlay 'evaporate t)
            (setq sdb/agent-shell-annotations
                  (append sdb/agent-shell-annotations (list annotation)))
            (deactivate-mark)
            (sdb/agent-shell-annotations--refresh-preview)
            (message "Annotation %d added (%d pending)"
                     id (length sdb/agent-shell-annotations))
            annotation))))))

(defun sdb/agent-shell-annotations--fence (text)
  "Return a Markdown fence longer than any backtick run in TEXT."
  (let ((length 3)
        (start 0))
    (while (string-match "`+" text start)
      (setq length (max length (1+ (- (match-end 0) (match-beginning 0))))
            start (match-end 0)))
    (make-string length ?`)))

(defun sdb/agent-shell-annotations--format-one (annotation)
  "Format ANNOTATION with its comment and numbered snapshot lines."
  (let* ((text (map-elt annotation :text))
         (fence (sdb/agent-shell-annotations--fence text))
         (line (map-elt annotation :first-line))
         (numbered
          (mapconcat
           (lambda (content)
             (prog1 (format "%d: %s" line content)
               (setq line (1+ line))))
           (split-string (string-remove-suffix "\n" text) "\n" nil)
           "\n")))
    (propertize
     (format "## Annotation %d\nSource: %S\nLines: %d-%d (at capture)\n\nComment:\n%s\n\n%stext\n%s\n%s"
             (map-elt annotation :id) (map-elt annotation :source)
             (map-elt annotation :first-line) (map-elt annotation :last-line)
             (map-elt annotation :comment) fence numbered fence)
     'sdb/agent-shell-annotation-id (map-elt annotation :id))))

(defun sdb/agent-shell-annotations--format-batch ()
  "Return one prompt containing all pending annotation snapshots."
  (unless sdb/agent-shell-annotations
    (user-error "No annotations pending"))
  (concat
   "Please review these annotated excerpts together.\n"
   "Excerpts and line numbers are snapshots captured when annotated; source buffers may have changed.\n\n"
   (mapconcat #'sdb/agent-shell-annotations--format-one
              sdb/agent-shell-annotations "\n\n")
   "\n"))

(defun sdb/agent-shell-annotations--read-annotation ()
  "Return the annotation at point, or prompt to choose a pending note."
  (unless sdb/agent-shell-annotations
    (user-error "No annotations pending"))
  (let* ((ids (delete-dups
               (delq nil
                     (cons (get-text-property (point) 'sdb/agent-shell-annotation-id)
                           (mapcar (lambda (overlay)
                                     (overlay-get overlay 'sdb/agent-shell-annotation-id))
                                   (overlays-at (point)))))))
         (matches (seq-filter (lambda (annotation)
                                (memq (map-elt annotation :id) ids))
                              sdb/agent-shell-annotations)))
    (if (= (length matches) 1)
        (car matches)
      (let ((choices
             (mapcar
              (lambda (annotation)
                (cons (format "%d: %s:%d-%d — %s"
                              (map-elt annotation :id) (map-elt annotation :source)
                              (map-elt annotation :first-line) (map-elt annotation :last-line)
                              (truncate-string-to-width
                               (replace-regexp-in-string "\n" " " (map-elt annotation :comment))
                               60 nil nil "…"))
                      annotation))
              (or matches sdb/agent-shell-annotations))))
        (cdr (assoc (completing-read "Annotation: " choices nil t) choices))))))

(defun sdb/agent-shell-edit-annotation (annotation comment)
  "Replace ANNOTATION's comment with COMMENT, keeping its snapshot."
  (interactive
   (let ((annotation (sdb/agent-shell-annotations--read-annotation)))
     (list annotation (read-string "Comment: " (map-elt annotation :comment)))))
  (unless (memq annotation sdb/agent-shell-annotations)
    (user-error "Annotation is no longer pending"))
  (sdb/agent-shell-annotations--require-comment comment)
  (setf (map-elt annotation :comment) comment)
  (when-let* ((overlay (map-elt annotation :overlay))
              ((overlay-buffer overlay)))
    (overlay-put overlay 'help-echo
                 (format "Note %d: %s" (map-elt annotation :id) comment)))
  (sdb/agent-shell-annotations--refresh-preview))

(defun sdb/agent-shell-remove-annotation (annotation)
  "Remove ANNOTATION and its highlight from the pending batch."
  (interactive (list (sdb/agent-shell-annotations--read-annotation)))
  (unless (memq annotation sdb/agent-shell-annotations)
    (user-error "Annotation is no longer pending"))
  (delete-overlay (map-elt annotation :overlay))
  (setq sdb/agent-shell-annotations (delq annotation sdb/agent-shell-annotations))
  (sdb/agent-shell-annotations--refresh-preview))

(defun sdb/agent-shell-clear-annotations ()
  "Clear pending annotations and remove their highlights."
  (interactive)
  (dolist (annotation sdb/agent-shell-annotations)
    (delete-overlay (map-elt annotation :overlay)))
  (setq sdb/agent-shell-annotations nil)
  (sdb/agent-shell-annotations--refresh-preview))

(defvar sdb/agent-shell-annotations-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "e") #'sdb/agent-shell-edit-annotation)
    (define-key map (kbd "d") #'sdb/agent-shell-remove-annotation)
    (define-key map (kbd "g") #'sdb/agent-shell-annotations--refresh-preview)
    (define-key map (kbd "C-c C-c") #'sdb/agent-shell-send-annotations-to)
    map)
  "Keymap for reviewing annotations before inserting them into a prompt.")

(define-derived-mode sdb/agent-shell-annotations-preview-mode special-mode "Agent-Notes"
  "Review collected excerpts and edit their comments.
Use e to edit, d to remove, g to refresh, and C-c C-c to transfer the
batch to an agent-shell prompt for review; this does not submit it.")

(defun sdb/agent-shell-annotations--refresh-preview ()
  "Refresh the annotation preview if its buffer already exists."
  (interactive)
  (when-let* ((buffer (get-buffer sdb/agent-shell-annotations--preview-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (position (point)))
        (erase-buffer)
        (insert (if sdb/agent-shell-annotations
                    (sdb/agent-shell-annotations--format-batch)
                  "No annotations pending.\n"))
        (goto-char (min position (point-max)))
        (set-buffer-modified-p nil)))))

(defun sdb/agent-shell-preview-annotations ()
  "Show the pending batch with commands to edit, remove, or transfer notes."
  (interactive)
  (unless sdb/agent-shell-annotations
    (user-error "No annotations pending"))
  (let ((buffer (get-buffer-create sdb/agent-shell-annotations--preview-buffer-name)))
    (with-current-buffer buffer
      (sdb/agent-shell-annotations-preview-mode))
    (sdb/agent-shell-annotations--refresh-preview)
    (pop-to-buffer buffer)))

(defun sdb/agent-shell-send-annotations-to ()
  "Insert the pending batch into a chosen existing agent-shell prompt.
The user reviews and submits the draft in agent-shell.  Annotations
are cleared only after successful insertion, and retained on errors
or cancellation.  A busy shell without a live prompt is rejected."
  (interactive)
  (let* ((text (substring-no-properties (sdb/agent-shell-annotations--format-batch)))
         (shell (agent-shell--read-shell-buffer :prompt "Send annotations to shell: ")))
    (unless (and (buffer-live-p shell)
                 (with-current-buffer shell
                   (and (derived-mode-p 'agent-shell-mode)
                        (map-nested-elt agent-shell--state '(:session :id)))))
      (user-error "Selected agent-shell session is not ready"))
    (unless (agent-shell--can-insert-into-prompt-p :shell-buffer shell)
      (user-error "Selected session is busy and has no live prompt; try again later"))
    (unless (agent-shell-insert :text text :shell-buffer shell :submit nil)
      (user-error "Batch was not inserted; annotations have been retained"))
    (sdb/agent-shell-clear-annotations)
    (message "Annotations inserted into the prompt for review; submit from agent-shell")))

(provide 'agent-shell-annotations)
;;; agent-shell-annotations.el ends here
