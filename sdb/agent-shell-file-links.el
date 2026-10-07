;;; agent-shell-file-links.el --- Native viewers and HTML plots -*- lexical-binding: t; -*-

;;; Commentary:
;; Route local HTML plots to a browser and image/PDF links to Emacs viewers.
;; HTML source citations retain line navigation.  Other links keep the
;; agent-shell defaults.  Run bin/test-agent-shell-file-links.sh for tests.

;;; Code:

(require 'agent-shell-markdown)
(require 'browse-url)
(require 'map)
(require 'url-util)

(defun sdb/agent-shell-file-links--parse (url)
  "Parse local URL, accepting percent escapes in file URLs.
Try the original URL first to preserve paths containing literal percent signs."
  (or (agent-shell-markdown--parse-local-link url)
      (when (string-prefix-p "file:" url)
        (agent-shell-markdown--parse-local-link (url-unhex-string url)))))

(defun sdb/agent-shell-file-links--open (original url)
  "Route local viewer links in URL, delegating other links to ORIGINAL."
  (let* ((parsed (sdb/agent-shell-file-links--parse url))
         (file (map-elt parsed :file))
         (extension (and file (downcase (or (file-name-extension file) "")))))
    (cond
     ((not (and file (not (file-remote-p file)) (file-regular-p file)))
      (funcall original url))
     ((member extension '("png" "jpg" "jpeg" "svg" "gif" "tif" "tiff"
                          "webp" "pdf"))
      ;; A binary-file check would send these to an external application.
      ;; Use the configured Emacs opener, letting normal-mode choose a viewer.
      (agent-shell-markdown-visit-file :file file)
      t)
     ((member extension '("html" "htm"))
      (if (map-elt parsed :line-start)
          (agent-shell-markdown-visit-file
           :file file
           :line-start (map-elt parsed :line-start)
           :line-end (map-elt parsed :line-end)
           :column (map-elt parsed :column))
        (browse-url-of-file file))
      t)
     (t (funcall original url)))))

(defun sdb/agent-shell-file-links-enable ()
  "Install local file routing for agent-shell Markdown links.
Repeated calls do not duplicate the advice."
  (advice-add 'agent-shell-markdown--open-local-link :around
              #'sdb/agent-shell-file-links--open))

(provide 'agent-shell-file-links)
;;; agent-shell-file-links.el ends here
