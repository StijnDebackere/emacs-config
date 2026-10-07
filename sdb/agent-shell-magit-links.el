;;; agent-shell-magit-links.el --- Staged-diff review links -*- lexical-binding: t; -*-

;;; Commentary:
;; Open magit:staged links as staged diffs for review.  An optional repo query
;; selects a local Git repository: magit:staged?repo=%2Fpath%2Fto%2Frepo.
;; The only supported action opens a view; it does not stage or commit.
;; Run bin/test-agent-shell-magit-links.sh for the isolated ERT suite.

;;; Code:

(require 'agent-shell-markdown)
(require 'subr-x)
(require 'url-util)

(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-diff-staged "magit-diff" (&optional rev args files))

(defun sdb/agent-shell-magit-links--directory (url)
  "Return the local directory selected by a supported Magit URL.
Accept `magit:staged' and `magit:staged?repo=ABSOLUTE-PATH'.  The latter
accepts percent escapes, including spaces.  Reject other actions or queries."
  (let ((case-fold-search t))
    (unless (string-match "\\`magit:staged\\(?:?repo=\\([^&#?]+\\)\\)?\\'" url)
      (user-error "Supported Magit links: magit:staged or magit:staged?repo=PATH"))
    (let* ((encoded (match-string 1 url))
           (directory
            (if encoded
                (progn
                  (unless (string-match-p "\\`\\(?:[^%]\\|%[[:xdigit:]]\\{2\\}\\)*\\'" encoded)
                    (user-error "Invalid percent escape in Magit repository path"))
                  (decode-coding-string (url-unhex-string encoded) 'utf-8))
              default-directory)))
      (unless (and (file-name-absolute-p directory)
                   (not (file-remote-p directory))
                   (not (string-match-p "[\0\n\r]" directory))
                   (file-directory-p directory))
        (user-error "Magit review links require an existing local directory"))
      (file-name-as-directory directory))))

(defun sdb/agent-shell-magit-links--open (original url)
  "Open supported Magit URL as a staged diff, delegating others to ORIGINAL."
  (if (string-prefix-p "magit:" url t)
      (let ((directory (sdb/agent-shell-magit-links--directory url)))
        (require 'magit-diff)
        (let ((default-directory
               (or (magit-toplevel directory)
                   (user-error "Directory is not a Git repository"))))
          ;; Explicit arguments avoid inherited revision or file filters.
          (magit-diff-staged nil nil nil)))
    (funcall original url)))

(defun sdb/agent-shell-magit-links--verb (original url verb)
  "Describe Magit URL accurately in link hints; preserve an explicit VERB.
Delegate other links to ORIGINAL."
  (if (and (not verb) (string-prefix-p "magit:" url t))
      "review staged changes in Magit"
    (funcall original url verb)))

(defun sdb/agent-shell-magit-links-enable ()
  "Install staged-diff links and hints in agent-shell Markdown.
Repeated calls do not duplicate the advice."
  (advice-add 'agent-shell-markdown--open-link :around
              #'sdb/agent-shell-magit-links--open)
  (advice-add 'agent-shell-markdown--link-verb :around
              #'sdb/agent-shell-magit-links--verb))

(provide 'agent-shell-magit-links)
;;; agent-shell-magit-links.el ends here
