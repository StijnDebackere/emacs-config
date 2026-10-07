;;; agent-shell-file-links-tests.el --- File routing tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise real Markdown parsing and navigation with external openers stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-file-links)

(defmacro sdb/agent-shell-file-links-test--fixture (&rest body)
  "Run BODY with sample files, capturing opens in `opens'."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "sdb-file-links-" t))
          (default-directory (file-name-as-directory directory))
          opens
          (agent-shell-markdown-open-file-function
           (lambda (file) (push (list 'emacs file) opens) nil)))
     (unwind-protect
         (progn
           (dolist (name '("plot.html" "plot.HTM" "plot with spaces.html"
                           "literal%20plot.html" "image.png" "image.PNG"
                           "image.jpg" "image.JPG" "image.jpeg" "image.JPEG"
                           "image.gif" "image.GIF" "image.tif" "image.TIF"
                           "image.tiff" "image.TIFF" "image.webp" "image.WEBP"
                           "image.svg" "image.SVG"
                           "document.pdf" "document.PDF" "source.el"
                           "binary.dat"))
             (with-temp-file (expand-file-name name directory)
               (if (member (downcase (or (file-name-extension name) ""))
                           '("png" "jpg" "jpeg" "gif" "tif" "tiff"
                             "webp" "pdf" "dat"))
                   (insert "binary\0data")
                 (if (equal (downcase (or (file-name-extension name) "")) "svg")
                     (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\">\n"
                             "<rect width=\"10\" height=\"10\" fill=\"red\"/>\n</svg>\n")
                   (insert "one\ntwo\nthree\nfour\n")))))
           (sdb/agent-shell-file-links-enable)
           (cl-letf (((symbol-function 'browse-url-of-file)
                      (lambda (file &rest _)
                        (push (list 'browser file) opens)))
                     ((symbol-function 'browse-url)
                      (lambda (url &rest _)
                        (push (list 'web url) opens)))
                     ((symbol-function 'agent-shell-markdown--open-externally)
                      (lambda (file)
                        (push (list 'external file) opens))))
             ,@body))
       (advice-remove 'agent-shell-markdown--open-local-link
                      #'sdb/agent-shell-file-links--open)
       (dolist (buffer (buffer-list))
         (when-let* ((file (buffer-file-name buffer)))
           (when (file-in-directory-p file directory)
             (kill-buffer buffer))))
       (delete-directory directory t))))

(ert-deftest sdb/agent-shell-file-links-html-browser ()
  (sdb/agent-shell-file-links-test--fixture
    (dolist (name '("plot.html" "plot.HTM"))
      (should (agent-shell-markdown--open-local-link name))
      (should (equal (pop opens)
                     (list 'browser (expand-file-name name)))))))

(ert-deftest sdb/agent-shell-file-links-native-binary-viewers ()
  (sdb/agent-shell-file-links-test--fixture
    ;; Verify actual NUL-containing fixtures bypass the binary heuristic.
    (dolist (name '("image.png" "image.PNG" "image.jpg" "image.JPG"
                    "image.jpeg" "image.JPEG" "image.gif" "image.GIF"
                    "image.tif" "image.TIF" "image.tiff" "image.TIFF"
                    "image.webp" "image.WEBP" "document.pdf" "document.PDF"))
      (should (agent-shell-markdown--binary-file-p (expand-file-name name)))
      (cl-letf (((symbol-function 'agent-shell-markdown--binary-file-p)
                 (lambda (&rest _) (ert-fail "Viewer sent to binary check"))))
        (should (agent-shell-markdown--open-local-link name)))
      (should (equal (pop opens) (list 'emacs (expand-file-name name)))))))

(ert-deftest sdb/agent-shell-file-links-svg-viewer ()
  (sdb/agent-shell-file-links-test--fixture
    ;; SVG is text, but should still reach the configured native viewer.
    (dolist (name '("image.svg" "image.SVG"))
      (should-not (agent-shell-markdown--binary-file-p (expand-file-name name)))
      (cl-letf (((symbol-function 'agent-shell-markdown--binary-file-p)
                 (lambda (&rest _) (ert-fail "SVG sent to binary check"))))
        (should (agent-shell-markdown--open-local-link name)))
      (should (equal (pop opens) (list 'emacs (expand-file-name name)))))))

(ert-deftest sdb/agent-shell-file-links-encoded-svg-url ()
  (sdb/agent-shell-file-links-test--fixture
    (rename-file "image.svg" "image with spaces.svg")
    (should (agent-shell-markdown--open-local-link
             (concat "file://" directory "/image%20with%20spaces.svg")))
    (should (equal opens
                   (list (list 'emacs
                               (expand-file-name "image with spaces.svg")))))))

(ert-deftest sdb/agent-shell-file-links-native-viewer-ignores-source-lines ()
  (sdb/agent-shell-file-links-test--fixture
    (dolist (name '("document.pdf" "image.png" "image.jpg" "image.jpeg"
                    "image.svg" "image.gif" "image.tif" "image.tiff" "image.webp"))
      (let (arguments)
        (cl-letf (((symbol-function 'agent-shell-markdown-visit-file)
                   (lambda (&rest args) (setq arguments args))))
          (should (agent-shell-markdown--open-local-link (concat name "#L20"))))
        (should (equal arguments (list :file (expand-file-name name))))))))

(ert-deftest sdb/agent-shell-file-links-paths-with-spaces ()
  (sdb/agent-shell-file-links-test--fixture
    (dolist (url '("plot with spaces.html"
                   "file:plot%20with%20spaces.html"))
      (should (agent-shell-markdown--open-local-link url))
      (should (equal (pop opens)
                     (list 'browser (expand-file-name "plot with spaces.html")))))
    (agent-shell-markdown--open-local-link
     (concat "file://" directory "/plot%20with%20spaces.html"))
    (should (equal opens
                   (list (list 'browser
                               (expand-file-name "plot with spaces.html")))))))

(ert-deftest sdb/agent-shell-file-links-literal-percent-path ()
  (sdb/agent-shell-file-links-test--fixture
    (agent-shell-markdown--open-local-link
     (concat "file://" directory "/literal%20plot.html"))
    (should (equal opens
                   (list (list 'browser
                               (expand-file-name "literal%20plot.html")))))))

(ert-deftest sdb/agent-shell-file-links-encoded-pdf-url ()
  (sdb/agent-shell-file-links-test--fixture
    (rename-file "document.pdf" "document with spaces.pdf")
    (agent-shell-markdown--open-local-link
     (concat "file://" directory "/document%20with%20spaces.pdf"))
    (should (equal opens
                   (list (list 'emacs
                               (expand-file-name "document with spaces.pdf")))))))

(ert-deftest sdb/agent-shell-file-links-html-source-range ()
  (sdb/agent-shell-file-links-test--fixture
    (save-window-excursion
      (let ((agent-shell-markdown-open-file-function
             (lambda (file)
               (switch-to-buffer (find-file-noselect file))
               (selected-window))))
        (agent-shell-markdown--open-local-link "plot.html#L2-L3")
        (should (equal buffer-file-name (expand-file-name "plot.html")))
        (should (= (line-number-at-pos) 2))
        (should (equal (buffer-substring (region-beginning) (region-end))
                       "two\nthree"))
        (should-not opens)))))

(ert-deftest sdb/agent-shell-file-links-html-source-column ()
  (sdb/agent-shell-file-links-test--fixture
    (save-window-excursion
      (let ((agent-shell-markdown-open-file-function
             (lambda (file)
               (switch-to-buffer (find-file-noselect file))
               (selected-window))))
        (agent-shell-markdown--open-local-link "plot.html:3:2")
        (should (= (line-number-at-pos) 3))
        (should (= (current-column) 1))
        (should-not opens)))))

(ert-deftest sdb/agent-shell-file-links-encoded-html-source ()
  (sdb/agent-shell-file-links-test--fixture
    (let (arguments)
      (cl-letf (((symbol-function 'agent-shell-markdown-visit-file)
                 (lambda (&rest args) (setq arguments args))))
        (agent-shell-markdown--open-local-link
         (concat "file://" directory "/plot%20with%20spaces.html#L2-L3")))
      (should (equal arguments
                     (list :file (expand-file-name "plot with spaces.html")
                           :line-start 2 :line-end 3 :column nil)))
      (should-not opens))))

(ert-deftest sdb/agent-shell-file-links-other-files-unchanged ()
  (sdb/agent-shell-file-links-test--fixture
    (agent-shell-markdown--open-local-link "source.el")
    (should (equal (pop opens) (list 'emacs (expand-file-name "source.el"))))
    (agent-shell-markdown--open-local-link "binary.dat")
    (should (equal (pop opens) (list 'external (expand-file-name "binary.dat"))))))

(ert-deftest sdb/agent-shell-file-links-web-and-missing-unchanged ()
  (sdb/agent-shell-file-links-test--fixture
    (dolist (url '("https://example.com/plot.html" "missing.html"))
      (should-not (agent-shell-markdown--open-local-link url))
      (should-not opens)
      (agent-shell-markdown--open-link url)
      (should (equal (pop opens) (list 'web url))))))

(ert-deftest sdb/agent-shell-file-links-directories-unchanged ()
  (sdb/agent-shell-file-links-test--fixture
    (make-directory "directory.html")
    (agent-shell-markdown--open-local-link "directory.html")
    (should (equal opens
                   (list (list 'emacs (expand-file-name "directory.html")))))))

(ert-deftest sdb/agent-shell-file-links-install-idempotent ()
  (sdb/agent-shell-file-links-test--fixture
    (let ((opener agent-shell-markdown-open-file-function)
          (count 0))
      (sdb/agent-shell-file-links-enable)
      (sdb/agent-shell-file-links-enable)
      (advice-mapc (lambda (function _properties)
                     (when (eq function #'sdb/agent-shell-file-links--open)
                       (cl-incf count)))
                   'agent-shell-markdown--open-local-link)
      (should (= count 1))
      (should (eq opener agent-shell-markdown-open-file-function))
      (agent-shell-markdown--open-local-link "plot.html")
      (should (= (length opens) 1)))))

(provide 'agent-shell-file-links-tests)
;;; agent-shell-file-links-tests.el ends here
