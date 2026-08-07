;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-

;; Maintainer: https://github.com/StijnDebackere

;;; Commentary:

;; Emacs init file pieced together from different places on the
;; internet, usually source links are included

;;; Code:

;;; Startup:
;;  --------

;; Keep native-comp warnings/errors out of your face (still logged to
;; *Async-native-compile-log*), and cap parallel compiler jobs so a
;; cold eln-cache doesn't saturate every core at once.
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-async-jobs-number 4)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package ships with Emacs itself; wire it to straight so a bare
;; `(use-package foo)' installs `foo' via straight, without needing `:ensure'.
(setq straight-use-package-by-default t)
(require 'use-package)
(require 'bind-key)

;;;; Server start-up
;; Start the emacs server so files are opened in the opened emacs instance.
(use-package server
  :straight nil
  :config
  (progn
    (if (not (server-running-p)) (server-start))))


;;; macOS / Environment
;;  --------------------

;;;; macOS defaults
(when (string-equal system-type "darwin")
  ;; delete files by moving them to the trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; set option as meta
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)

  ;; adjust mouse scrolling down
  (setq mouse-wheel-scroll-amount (quote (0.01)))

  ;; Don't make new frames when opening a new file with Emacs
  (setq ns-pop-up-frames nil)

  ;; Not going to use these commands
  (put 'ns-print-buffer 'disabled t)
  (put 'suspend-frame 'disabled t))

;;;; Finder / iTerm helpers
(defun open-dir-in-finder ()
  "Open a new Finder window to the path of the current buffer."
  (interactive)
  (start-process "sdb-open-dir-process" nil "open" "."))

(defun open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/usr/local/Caskroom/iterm2/1.0.0/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (start-process "sdb-open-dir-process" nil "open" "-a" iterm-path ".")))

(bind-key "C-c o f" 'open-dir-in-finder)
(bind-key "C-c o t" 'open-dir-in-iterm)

;;;; path loading
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))


;;; File & buffer utilities
;;  ------------------------

;; https://github.com/syl20bnr/spacemacs/blob/0bbb4/layers/spacemacs/spacemacs-defaults/funcs.el#L779-L787
(defun sdb--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun sdb/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let* ((file-path (sdb--file-path))
            (file-name (file-name-nondirectory file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))


;;; Sane defaults
;; Amalgamation of
;; - https://github.com/magnars/.emacs.d/blob/master/settings/sane-defaults.el
;; - http://pages.sachachua.com/.emacs.d/Sacha.html

;; Save layout and reload upon closing/restarting
(desktop-save-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; ibuffer is improved version of list-buffers
(defalias 'list-buffers 'ibuffer)

;; We start in the home directory
(cd "~")
;; ensure that the `default-directory' matches the directory of the opened file
(add-hook 'find-file-hook
          (lambda ()
            (when (buffer-file-name)
              (setq default-directory
                    (file-name-directory (buffer-file-name))))))


;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Turn off the blinking cursor
(blink-cursor-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; Add newlines
(setq next-line-add-newlines t)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable highlighting in documents
(global-hi-lock-mode 1)

;; show matching parens and set no delay
(setq show-paren-delay 0)
(show-paren-mode t)

;; show column & line numbers
(column-number-mode t)
(line-number-mode t)

;; wrap lines in buffer
(global-visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

;; Show time in emacs
(setq display-time-24hr-format 1)
(display-time-mode 1)

;; Do not ask to kill processes
(setq confirm-kill-processes nil)

;; Ask to kill emacs
(setq confirm-kill-emacs 'y-or-n-p)

;;;; Backups
;; make back-ups to the ~/.emacs.d/backups directory
;; https://stackoverflow.com/q/151945/
(defvar --backup-directory (concat user-emacs-directory "backups"))
(defvar --auto-save-directory (concat user-emacs-directory "autosaves"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(if (not (file-exists-p --auto-save-directory))
        (make-directory --auto-save-directory t))

(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is
                                        ; saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new
                                        ; numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new
                                        ; numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-file-name-transforms `((".*" ,--auto-save-directory t))
      auto-save-timeout 20              ; number of seconds idle time before
                                        ; auto-save (default: 30)
      auto-save-interval 300            ; number of keystrokes between auto-saves
                                        ; (default: 300)
      fill-column 100)


;;;; automatic whitespace removal
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;;;; save file-visiting buffers after a period of idle time
;; native replacement for super-save
(auto-save-visited-mode 1)

;;;; electric-pair-mode
;; native replacement for smartparens' auto-pairing (incl. wrapping an
;; active region when typing an opening delimiter, given `delete-selection-mode'
;; is also on above)
(use-package elec-pair
  :straight nil
  :config
  (electric-pair-mode 1)
  :custom
  (electric-pair-preserve-balance t))

;;;; auto-revert-mode
;; autorevert buffer upon file changes
(use-package autorevert
  :straight nil
  :config
  (global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;;;; dired
(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("<backspace>" . dired-up-directory)
              ("b" . dired-up-directory)
              ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))


;;; Window & popup management

;;;; popper
(use-package popper
  :bind (("C-'"   . popper-toggle)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;;;; ace-window
(use-package ace-window
  :defer 1
  :preface
  (defun sdb/ace-window-dispatch (arg)
    "Call `ace-window', showing the `aw-dispatch-alist' shortcuts first
when there are more than 2 windows to choose from."
    (interactive "p")
    (if (> (length (aw-window-list)) 2)
        (aw-show-dispatch-help)
      (ace-window arg)))
  (defun sdb/ace-window-set-faces (&rest _)
    "Set ace-window's own faces.
Several themes (material, gotham) define `aw-leading-char-face' and/or
`aw-mode-line-face' themselves, silently overriding this customization
whenever they're (re-)enabled -- so this is also hooked into
`enable-theme-functions', not just called once here."
    (when (facep 'aw-leading-char-face)
      ;; see https://github.com/abo-abo/ace-window/issues/44#issuecomment-264923922
      (set-face-attribute 'aw-leading-char-face nil
                          :foreground "deep sky blue"
                          :background nil
                          :weight 'bold
                          :height 10.0)
      (set-face-attribute 'aw-mode-line-face nil
                          :inherit 'mode-line-buffer-id
                          :foreground "indian red")))
  :bind
  ("M-o" . sdb/ace-window-dispatch)
  :config
  (sdb/ace-window-set-faces)
  (add-hook 'enable-theme-functions #'sdb/ace-window-set-faces)
  (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always nil
        aw-dispatch-alist
        '((?x aw-delete-window     "Ace - Delete Window")
          (?c aw-swap-window       "Ace - Swap Window")
          (?h aw-split-window-vert "Ace - Split Vert Window")
          (?v aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?b balance-windows      "Ace - Balance Windows")))
  (ace-window-display-mode t))


;;;; windmove
;; hjkl -> shifted to right
;; h: left, j: down, k: up, l:right
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-;") 'windmove-right)
(global-set-key (kbd "s-l") 'windmove-up)
(global-set-key (kbd "s-k") 'windmove-down)

;;;; buffer-move
;; Move buffers between windows.

(use-package buffer-move
  :bind
  (("<C-M-S-up>" . 'buf-move-up)
   ("<C-M-S-down>" . 'buf-move-down)
   ("<C-M-S-left>" . 'buf-move-left)
   ("<C-M-S-right>" . 'buf-move-right)))


;;;; window splitting

(defun sdb/vsplit-other-window ()
  "Split window vertically and switch to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))
(defun sdb/hsplit-other-window ()
  "Split window horizontally and switch to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))
(defun sdb/toggle-window-split ()
  "Toggle between horizontal and vertical orientation for 2 windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))


;;;; switch-to-minibuffer
(defun sdb/switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


;;;; custom shortcuts
(bind-key "C-x 2" 'sdb/vsplit-other-window)
(bind-key "C-x 3" 'sdb/hsplit-other-window)
(bind-key "C-x |" 'toggle-window-split)
(bind-key "<C-S-down>" 'shrink-window)
(bind-key "<C-S-up>" 'enlarge-window)
(bind-key "<C-S-left>" 'shrink-window-horizontally)
(bind-key "<C-S-right>" 'enlarge-window-horizontally)
(bind-key "<f10>" 'sdb/switch-to-minibuffer-window)
;; the fullscreen of frame.el makes me lose my menu bar
(global-unset-key (kbd "<f11>"))


;;; AI / MCP tooling
(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el" :files ("*.el" "*.org"))
  ;; :custom (mcp-hub-servers
  ;;          `(("filesystem" . (:command "npx"
  ;;                             :args ("-y" "@modelcontextprotocol/server-filesystem")
  ;;                             :roots ("/home/lizqwer/MyProject/")))
  ;;            ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))))
  :config (require 'mcp-hub))
  ;; :hook (after-init . mcp-hub-start-all-server))


;;; Editing & navigation

;;;; sexp navigation
;; explicit bindings mirroring the old `sp-smartparens-bindings' scheme,
;; using Emacs's built-in sexp commands. Most of these already match the
;; vanilla defaults; bound explicitly here so they don't depend on that.
;; Keys that had no native equivalent (unwrap, slurp/barf, splice, symbol
;; nav) are intentionally left alone: C-M-a/C-M-e/C-M-w/C-<right>/C-<left>/
;; C-]/C-S-<backspace> revert to their vanilla Emacs bindings (beginning-
;; /end-of-defun, append-next-kill, right-word/left-word, abort-recursive-
;; edit, kill-whole-line); M-<delete>, M-D, C-M-], M-F, M-B, C-S-d, C-S-a
;; become unbound.
(bind-key "C-M-f" 'forward-sexp)
(bind-key "C-M-b" 'backward-sexp)
(bind-key "C-M-d" 'down-list)
(bind-key "C-M-u" 'backward-up-list)
(bind-key "C-M-n" 'forward-list)
(bind-key "C-M-p" 'backward-list)
(bind-key "C-M-k" 'kill-sexp)
(bind-key "C-M-SPC" 'mark-sexp)

;;;; buffer navigation
(defun sdb/push-mark-no-activate ()
  "Push `point' to `mark-ring' and do not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is
disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; copied from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun sdb/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;;; shortcuts
(bind-key "M-`" 'sdb/push-mark-no-activate)
(bind-key "s-`" 'consult-mark)
(bind-key "C-a" 'sdb/smarter-move-beginning-of-line)

;;;; avy
(use-package avy
  :bind ("C-." . avy-goto-char))

(use-package avy-zap
  :bind ("M-z" . avy-zap-to-char-dwim))

;;;; function navigation
(bind-key "C-M-S-a" 'beginning-of-defun)
(bind-key "C-M-S-e" 'end-of-defun)


;;;; Text manipulation
;; ;; Disabled because it annoys me in COMMIT_MSG and yml files...
;; (defun sdb/enable-dead-keys ()
;;   "Enable dead key expansion with TeX input method in text mode."
;;   (activate-input-method "TeX"))
;; (add-hook 'text-mode-hook 'sdb/enable-dead-keys)

(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Take a multi-line paragraph/REGION and make it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            ;; This would override `fill-column' if it's an integer.
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        ;; ;; including this prevents some crazy jumping inside brackets
        ;; ;; which is annoying in elisp
        ;; (goto-char end)
        (next-logical-line)))

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start end)))))

;;;;; shortcuts
(bind-key "C-w" 'cut-line-or-region)
(bind-key "M-w" 'copy-line-or-region)
(bind-key "C-M-q" 'unfill-paragraph)
(bind-key "M-;" 'comment-or-uncomment-region-or-line)
(bind-key "M-RET" 'comment-indent-new-line)

;;;; combobulate
(use-package treesit
  :straight nil
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars))

;; Do not forget to customize Combobulate to your liking:
;;
;;  M-x customize-group RET combobulate RET
;;
(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))

;;;; multiple-cursors
(use-package multiple-cursors
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-M->" . mc/mark-next-like-this-word)
  ("C-M-<" . mc/mark-previous-like-this-word)
  ("C-c C-." . mc/mark-all-like-this-dwim)
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C-c #" . mc/insert-numbers)
  :custom
  (mc/insert-numbers-default 1))

;;;; visual-regexp
;; https://github.com/benma/visual-regexp-steroids.el/issues/21
;; to get visual-regexp-steroids to work:
;; change vr--command-python-default python -> python3, no unicode errors
;; need to run python3 for *Packages* error
(use-package visual-regexp-steroids
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace))


;;; UI chrome

;;;; minions
(use-package minions
  :config
  (minions-mode 1))

;;;; outline
;; Emacs's own outline.el has provided cycling, subtree movement, and
;; promote/demote natively since Emacs 29, making outline-magic redundant.
;;;;; TODO:
;; - Come up with different shortcuts that do not result in me accidentally
;;   promoting and demoting LaTeX sections
(use-package outline
  :straight nil
  :bind (:map outline-minor-mode-map
              ("C-<tab>" . outline-cycle)
              ("M-<up>" . outline-move-subtree-up)
              ("M-<down>" . outline-move-subtree-down)
              ("M-<left>" . outline-promote)
              ("M-<right>" . outline-demote)
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading))
  :hook ((LaTeX-mode . outline-minor-mode)
         ;; taken from the example in outline-magic
         (LaTeX-mode . (lambda ()
                         (setq outline-promotion-headings
                               '("\\chapter"
                                 "\\section"
                                 "\\subsection"
                                 "\\subsubsection"
                                 "\\paragraph"
                                 "\\subparagraph"))))))


;;; Version control

;;;; magit
(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-c f" . magit-file-dispatch)
   ("C-c g" . magit-dispatch))
  ;; Add a suffix to an existing transient
  :custom
  (magit-log-arguments (quote ("--decorate" "-n256")))
  (magit-refresh-status-buffer nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

;;;; forge
;; See https://docs.magit.vc/forge/Setup-for-Githubcom.html
(setq auth-sources '("~/.authinfo"))
(use-package forge
  :after magit
  :bind
  ("C-c n" . forge-dispatch))

;;;; pr-review
(use-package pr-review
  :straight (:host github :repo "blahgeek/emacs-pr-review" :files ("*.el" "graphql"))
  :after (magit forge)
  ;; see https://gitlab.com/magus/mes/-/blob/86153/lisp/mes-dev-basics.el#L76
  :config
  (load-file (expand-file-name "~/.emacs.d/sdb/pr-review-mods.el"))
  ;; Customize your settings
  (setq pr-review-main-branch-name "main")
  (setq pr-review-repo-base-dir "~/repos")
  (setq pr-review-ghub-auth-name 'forge)
  (transient-define-prefix pr-review-dispatch ()
    "Main dispatch menu for your-mode"
    ["Actions"
     ("e" "Comment" pr-review-context-comment)
     ("d" "Ediff" pr-review-ediff-with-main)
     ("a" "Action" pr-review-context-action)
     ("R" "Refresh" pr-review-refresh)
     ("c" "Submit" pr-review-submit-review)
     ("o" "Open in browser" pr-review-open-in-default-browser)])
  :bind
  ("C-c j" . pr-review-jump-to-file-in-pr)
  ("C-c c" . pr-review-comment-on-region)
  (:map magit-mode-map
        ("C-c r" . pr-review-from-forge))
  (:map pr-review-mode-map
        ("?" . pr-review-dispatch)
        ("RET" . pr-review-visit-file)
        ("e" . pr-review-context-comment)
        ("d" . pr-review-ediff-with-main)
        ("a" . pr-review-context-action)
        ("R" . pr-review-refresh)
        ("c" . pr-review-submit-review)
        ("o" . pr-review-open-in-default-browser)))

;;;; ediff
(use-package ediff
  :straight nil
  :bind
  ("C-c e" . ediff-files)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :hook
  (ediff-before-setup . sdb/store-pre-ediff-winconfig)
  (ediff-quit . sdb/restore-pre-ediff-winconfig))

;; Restore window configuration after ediff:
;; Source: http://emacs.stackexchange.com/a/17089
(defvar sdb/ediff-last-windows nil)
(defun sdb/store-pre-ediff-winconfig ()
  "Store window configuration before ediff call."
  (setq sdb/ediff-last-windows (current-window-configuration)))
(defun sdb/restore-pre-ediff-winconfig ()
  "Restore saved window configuration after ediff ends."
  (set-window-configuration sdb/ediff-last-windows))


;;; tramp
(use-package tramp
  :straight nil
  :demand
  :config
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config"))))


;;; Search

;;;; ripgrep
(use-package rg)

;;;; wgrep
(use-package wgrep)


;;; projectile:
(use-package projectile
  :bind
  (:map projectile-mode-map
        ("s-p" . 'projectile-command-map))
  :init
  (projectile-mode t))

;; consult UI (preview, multi-source) on top of projectile's own project-root
;; detection, rather than `consult-project-buffer's native `project.el' one
(use-package consult-projectile
  :after (projectile consult)
  :bind
  (:map projectile-command-map
        ("b" . consult-projectile-switch-to-buffer)))


;;; Completion (vertico + consult + marginalia + orderless + embark)
;; replaces ivy/counsel/swiper/ivy-rich: same completion-in-minibuffer job,
;; but built directly on Emacs's own `completing-read' instead of a bespoke
;; engine, so any command that uses standard completion benefits, not just
;; the ones with a dedicated counsel-* wrapper.
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; ido-like directory navigation in the minibuffer -- bundled with vertico
;; itself, not a separate package
(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; resume the last completion session (e.g. re-open the last `consult-ripgrep'
;; exactly where it was left) -- also bundled with vertico itself
(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat))

(savehist-mode 1)
(recentf-mode 1)
(setq enable-recursive-minibuffers t)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; orderless first so `find-file' gets space-separated/out-of-order
  ;; matching too; partial-completion and basic stay as fallbacks (basic in
  ;; particular is what makes TRAMP hostname completion, e.g. `/ssh:`, work)
  (completion-category-overrides '((file (styles orderless partial-completion basic)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :bind
  ;; unifies buffers/recentf/bookmarks -- direct replacement for
  ;; `ivy-use-virtual-buffers'
  ("C-x b" . consult-buffer)
  ;; use consult-line instead of isearch
  ;; word at point is offered as the first M-n history entry, same as swiper
  ("C-s" . consult-line)
  ("M-y" . consult-yank-pop)
  ("C-c C-t" . consult-outline)
  ;; If called with prefix argument, directory and args can be provided
  ("C-c s" . consult-ripgrep)
  ;; remap rather than a direct key so it takes over both of `goto-line's
  ;; default bindings (M-g g and M-g M-g)
  ([remap goto-line] . consult-goto-line))

;; per-candidate actions (kill/rename/other-frame buffer, etc.) and
;; multi-candidate marking -- replaces the custom `ivy-toggle-mark' /
;; `ivy-set-actions' code, using embark's own built-in action set instead
;; of hand-written ones
(use-package embark
  :bind
  ("C-;" . embark-act)
  ;; turns the current candidate list into an editable buffer, matching the
  ;; old `ivy-occur' workflow. Which mode you land in -- and how to make it
  ;; editable -- depends on the source command:
  ;; - `consult-ripgrep'/grep results -> `grep-mode', use `wgrep':
  ;;   C-c C-p to make it editable, C-c C-e to apply the changes to the
  ;;   actual files, C-c C-k to discard instead
  ;; - `consult-line' results -> `occur-mode' (built into Emacs):
  ;;   e to enter `occur-edit-mode', C-c C-c to apply the changes back to
  ;;   the original buffer
  ("C-c C-o" . embark-export)
  ("C-h B" . embark-bindings)
  (:map vertico-map
        ;; toggle-select the candidate at point directly, same muscle memory
        ;; as the old `ivy-toggle-mark'
        ("C-SPC" . embark-select)
        ;; matches ivy's own M-o action-dispatch key; scoped to the
        ;; minibuffer so the global M-o -> ace-window binding is untouched
        ("M-o" . embark-act)
        ;; act on the whole `embark-select' selection instead of just the
        ;; candidate at point; with an empty selection this acts on every
        ;; candidate shown, hence the separate key rather than reusing M-o
        ("M-O" . embark-act-all)))

(use-package embark-consult
  :after (embark consult))


;;; Themes & appearance
(defalias 'switch-theme 'consult-theme)

;; # You may need to run these two lines if you haven't set up Homebrew
;; # Cask and its fonts formula.
;; brew install caskroom/cask/brew-cask
;; brew tap caskroom/fonts

;; brew cask install font-source-code-pro
(add-to-list 'default-frame-alist
             (cond
              ((string-equal system-type "darwin")    '(font . "Iosevka Comfy 18"))
              ((string-equal system-type "gnu/linux") '(font . "Iosevka Comfy 14"))))

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;; Icons
(use-package nerd-icons
  :config
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))

;;;; Default theme
(use-package material-theme
  :config
  (load-theme 'material t))
(use-package dracula-theme)
(use-package solarized-theme)
(use-package gotham-theme)


;;; Programming support

;;;; company-mode
;; Setup company-mode for autocompletion

(use-package company
  :hook
  (prog-mode . company-mode)
  :bind (:map company-active-map
              ;; from https://github.com/company-mode/company-mode/issues/246#issuecomment-65064467
              ;; pressing TAB twice results in autocompletion of the selected item
              ("TAB" . company-complete)
              ;; unbind return from completion
              ("<tab>")
              ("RET")
              ("<return>"))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.05)
  (company-show-numbers t))

(use-package company-box
  :hook (company-mode . company-box-mode))

;;;; GitHub CoPilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  ;; :hook (prog-mode . (lambda ()
  ;;                      (unless (derived-mode-p 'sql-mode))
  ;;                      copilot-mode))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("TAB" . copilot-accept-completion))
  )

;;;; Flycheck
(use-package flycheck
  :init
  (global-flycheck-mode))

;;;; lsp
;; recommendations in https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 64000000)

(use-package lsp-mode
  ;; do not show yas when lsp-mode enabled
  ;; :diminish (yas-minor-mode . "")
  ;; :after (yasnippet)
  :hook
  (
   (sh-mode . lsp-mode)
   (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'sql-mode)
                          (lsp-deferred))))
   (text-mode . (lambda ()
                  ;; If something enabled lsp-mode for this buffer, turn it off.
                  (when (bound-and-true-p lsp-mode)
                    (lsp-mode -1)))))
  ;; enable yas-minor-mode on lsp-mode to fix completion error
   ;; (lsp-mode . yas-minor-mode))
  :bind
  ("s-l" . lsp-keymap-prefix)
  ("M-<tab>" . lsp-execute-code-action)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-ruff-server-command '("ruff" "server"))
  (lsp-disabled-clients '(sql-ls))
  (lsp-prefer-capf t)
  (lsp-idle-delay 0.0)
  (lsp-enable-snippet nil)
  (lsp-modeline-code-actions-mode 1)
  (lsp-auto-execute-action nil)
  ;; (lsp-auto-guess-root t)
  ;; buffers like plain `lisp-mode', `special-mode', or `makefile-bsdmake-mode'
  ;; have no configured lsp client at all, so this warning is just noise
  (lsp-warn-no-matched-clients nil))

;; to make this work, run npm install -g pyright
(use-package lsp-pyright
  :init
  ;; see https://github.com/emacs-lsp/lsp-pyright/issues/66#issuecomment-1144136538
  ;; this will start a separate process for each lsp
  (setq lsp-pyright-multi-root nil)
  :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))
  ;; [DEPRECATED] for lsp-ruff, make sure to install ruff-lsp somewhere on the
  ;; exec-path => now use `ruff server` directly

(defun consult-lsp-symbols-or-imenu (arg)
  "Use `consult-lsp-symbols' on ARG if `lsp-mode' is active, else `consult-imenu'."
  (interactive "P")
  (if lsp-mode
      (consult-lsp-symbols arg)
    (consult-imenu)))

(use-package consult-lsp
  :commands consult-lsp-symbols
  :bind ("M-i" . consult-lsp-symbols-or-imenu)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(bind-key "C-c C-j" 'consult-imenu)

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  ("s->" . lsp-ui-find-next-reference)
  ("s-<" . lsp-ui-find-prev-reference)
  ;; scoped to lsp-ui's own minor-mode map, not global -- major-mode maps
  ;; take precedence over the global map, and e.g. `python-mode-map' already
  ;; claims C-c C-d for `python-describe-at-point'
  (:map lsp-ui-mode-map
        ("C-c C-d" . lsp-ui-doc-glance))  ;; default ("s-l h g")
  :custom
  (lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))


;;; Language-specific modes

;;;; LaTeX
;; LaTeX environment in Emacs. Work in progress.
(use-package reftex
  :straight nil
  :defer t
  :custom
  (reftex-plug-into-auctex t))
(use-package tex
  ;; to get working: https://github.com/jwiegley/use-package/issues/379
  :straight auctex
  :defer t
  :config
  ;; latexmk document compilation
  ;; see http://tex.stackexchange.com/q/10561
  (add-to-list 'TeX-command-list '("LaTeX Make" "latexmk -lualatex -f %t" TeX-run-TeX))
  (add-to-list 'TeX-command-list '("View" "open %s.pdf" TeX-run-command))
  (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "LaTeX Make")))

  ;; enable folding of environments
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (TeX-fold-mode 1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'TeX-after-compilation-finished-functions #'sdb/tex-close-TeX-buffer)

  ;; Close tex-output buffer if there are only warnings after compilation
  ;; see https://emacs.stackexchange.com/q/38258/
  (defcustom TeX-buf-close-at-warnings-only t
    "Close TeX buffer if there are only warnings."
    :group 'TeX-output
    :type 'boolean)

  :custom
  ;; parse tex files on load and save
  (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-auto-cache-preamble 1)
  ;; do not change font height or width in latex files
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 'color))

(defun sdb/tex-close-TeX-buffer (_output)
  "Close compilation buffer if there are no errors.
Hook this function into `TeX-after-compilation-finished-functions'."
  (let ((buf (TeX-active-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (progn (TeX-parse-all-errors)
                     (or
                      (and TeX-buf-close-at-warnings-only
                           (null (cl-assoc 'error TeX-error-list)))
                      (null TeX-error-list)))
          (cl-loop for win in (window-list)
                   if (eq (window-buffer win) (current-buffer))
                   do (kill-buffer (window-buffer win))))))))

;;;; Lua
;; Requires lua and luarocks installations, available through Homebrew

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))


;;;; elisp
(use-package eldoc)


;;; Custom:
;;  -------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb"
           "#81d4fa" "#263238"))
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(copilot-indent-offset-warning-disable t)
 '(custom-safe-themes
   '("afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477"
     default))
 '(fci-rule-color "#ECEFF1")
 '(flycheck-checker-error-threshold 1000)
 '(hl-sexp-background-color "#efebe9")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#B71C1C") (40 . "#FF5722") (60 . "#FFA000") (80 . "#558b2f")
     (100 . "#00796b") (120 . "#2196f3") (140 . "#4527A0")
     (160 . "#B71C1C") (180 . "#FF5722") (200 . "#FFA000")
     (220 . "#558b2f") (240 . "#00796b") (260 . "#2196f3")
     (280 . "#4527A0") (300 . "#B71C1C") (320 . "#FF5722")
     (340 . "#FFA000") (360 . "#558b2f")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-log-types '((treesit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vc-annotate-color-map '((20 . "#B71C1C") (40 . "#FF5722") (60 . "#FFA000") (80 . "#558b2f") (100 . "#00796b") (120 . "#2196f3") (140 . "#4527A0") (160 . "#B71C1C") (180 . "#FF5722") (200 . "#FFA000") (220 . "#558b2f") (240 . "#00796b") (260 . "#2196f3") (280 . "#4527A0") (300 . "#B71C1C") (320 . "#FF5722") (340 . "#FFA000") (360 . "#558b2f"))))
