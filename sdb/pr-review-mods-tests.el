;;; pr-review-mods-tests.el --- ERT tests for pr-review-mods -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the pure helper functions in `pr-review-mods', added
;; after an upstream emacs-pr-review update silently changed the shape
;; of the diff-line text properties and of `pr-review-url-parse''s
;; return value, breaking `pr-review-jump-to-file-in-pr' and
;; `pr-review-from-forge'.
;;
;; This file is never `require'd anywhere in init.el, so it never
;; loads or runs during normal Emacs startup. Run it on demand with:
;;   bin/test-pr-review-mods.sh
;;
;; The diff-prop tests use a hand-built fixture matching the alist
;; shape emacs-pr-review's `pr-review--insert-diff' currently attaches
;; as the `pr-review-diff-line-left'/`pr-review-diff-line-right' text
;; properties. If upstream changes that shape again, these tests won't
;; catch it automatically -- update the fixture here to match, which
;; is also the reminder to check `pr-review--diff-prop-path'/`-line'
;; still read it correctly.
;;
;; The pr-url tests call the real upstream `pr-review-url-parse', so
;; they do catch a future change to *that* function's output shape.

;;; Code:
(require 'ert)
(require 'pr-review-mods)

(defconst pr-review-mods-test--diff-prop
  '((path . "foo/bar.el")
    (path-orig . "foo/bar.el")
    (line . 42))
  "Fixture matching the current `pr-review-diff-line-{left,right}' shape.")

(defconst pr-review-mods-test--diff-prop-renamed
  '((path . "new-name.el")
    (path-orig . "old-name.el")
    (line . 7))
  "Fixture for a renamed file, where path and path-orig differ.")

(ert-deftest pr-review-mods-test-diff-prop-path ()
  "`pr-review--diff-prop-path' reads the current path out of the property."
  (should (equal (pr-review--diff-prop-path pr-review-mods-test--diff-prop)
                 "foo/bar.el")))

(ert-deftest pr-review-mods-test-diff-prop-line ()
  "`pr-review--diff-prop-line' reads the line number out of the property."
  (should (= (pr-review--diff-prop-line pr-review-mods-test--diff-prop) 42)))

(ert-deftest pr-review-mods-test-diff-prop-line-is-a-number ()
  "The line number must be a number, not a sub-alist.
This is the exact shape mismatch that crashed
`pr-review-jump-to-file-in-pr' with wrong-type-argument
number-or-marker-p: `(cdr right-prop)' returned the alist tail
instead of the line number."
  (should (numberp (pr-review--diff-prop-line pr-review-mods-test--diff-prop))))

(ert-deftest pr-review-mods-test-diff-prop-renamed-file ()
  "Path and line both read correctly when path and path-orig differ."
  (should (equal (pr-review--diff-prop-path pr-review-mods-test--diff-prop-renamed)
                 "new-name.el"))
  (should (= (pr-review--diff-prop-line pr-review-mods-test--diff-prop-renamed) 7)))

(ert-deftest pr-review-mods-test-parse-pr-url-github ()
  "`pr-review--parse-pr-url' extracts host/owner/repo/number from a GitHub URL."
  (let ((pr-ref (pr-review--parse-pr-url
                 "https://github.com/klar-mx/analytics-klar-dbt/pull/9405")))
    (should pr-ref)
    (should (equal (plist-get pr-ref :host) "github.com"))
    (should (equal (plist-get pr-ref :owner) "klar-mx"))
    (should (equal (plist-get pr-ref :repo) "analytics-klar-dbt"))
    (should (= (plist-get pr-ref :number) 9405))))

(ert-deftest pr-review-mods-test-parse-pr-url-gitlab-merge-request ()
  "`pr-review--parse-pr-url' also handles GitLab merge-request URLs."
  (let ((pr-ref (pr-review--parse-pr-url
                 "https://gitlab.com/owner/repo/-/merge_requests/12")))
    (should pr-ref)
    (should (equal (plist-get pr-ref :host) "gitlab.com"))
    (should (equal (plist-get pr-ref :owner) "owner"))
    (should (equal (plist-get pr-ref :repo) "repo"))
    (should (= (plist-get pr-ref :number) 12))))

(ert-deftest pr-review-mods-test-parse-pr-url-invalid ()
  "`pr-review--parse-pr-url' returns nil for a non-PR URL."
  (should (null (pr-review--parse-pr-url
                 "https://github.com/klar-mx/analytics-klar-dbt"))))

(provide 'pr-review-mods-tests)
;;; pr-review-mods-tests.el ends here
