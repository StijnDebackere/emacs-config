;;; init.el --- Emacs init file

;; Maintainer: https://github.com/StijnDebackere

;;; Commentary:

;; Emacs init file pieced together by Stijn Debackere from
;; different places on the internet, usually source links are included
;; TODO
;; - look into epkg + borg setup to use github packages instead of elpa

;;; Code:
;;  -----


;;; Startup:
;;  --------

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))
(package-initialize)


;;;; use-package.el
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (or (package-installed-p 'use-package)
            (package-installed-p 'diminish))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;; From use-package README
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)

;;;; Server start-up
;; Start the emacs server so files are opened in the opened emacs instance.
(use-package server
  :config
  (progn
    (if (not (server-running-p)) (server-start)))
)

;;;; Mac customization
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
  (put 'suspend-frame 'disabled t)

  )

;;;;; path loading
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  )

;;;;; send dir to Finder and Iterm
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


;;;; Sane defaults
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
(diminish 'visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

;; Show time in emacs
(setq display-time-24hr-format 1)
(setq display-time-default-load-average nil)
(display-time-mode 1)

;; Do not ask to kill processes
(setq confirm-kill-processes nil)

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
      fill-column 100
      )

(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode)
  )

(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode +1))

;;; smartparens:
;;  ------------
(use-package smartparens
  :diminish smartparens-mode
  :init
  (sp-use-smartparens-bindings)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  ;; prefer this command as backward-kill-word
  (unbind-key "M-<backspace>" smartparens-mode-map)
  :custom
  (sp-show-pair-from-inside nil)
  )


;;; outline-magic:
;;  --------------

;; outline-magic simplifies many of the commands in outline-mode, easy
;; cycling, navigation, promotion and demotion of headings in documents
;;;; TODO:
;; - Come up with different shortcuts that do not result in me accidentally
;;   promoting and demoting LaTeX sections
(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :config
  (use-package outline-magic
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
                                   "\\subparagraph")
                                 )
                           )
                       )
           )
    )
)



;;; flyspell:
;;  ---------
;; setup flyspell
;;;; TODO
;; - ispell-hunspell-dict-paths-alist contains right dictionaries, but
;;   ispell-hunspell-dictionary-alist does not seem to load them
;; - check https://facility9.com/2015/01/unsetting-key-bindings-in-emacs/
(use-package flyspell
  :diminish (flyspell-mode . "")
  :hook
  (text-mode . flyspell-mode)
  (org-mode . flyspell-mode)
  (TeX-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (emacs-lisp-mode . flyspell-prog-mode)
  (python-mode . flyspell-prog-mode)
  :bind
  ("C-c w" . my/flyspell-check-next-highlighted-word)
  ("C-c b" . flyspell-buffer)
  :custom
  ;; (setenv "LANG" "en_GB.UTF-8")
  (progn (cond
          ((executable-find "aspell")
           ;; you may also need `ispell-extra-args'
           (setq ispell-program-name "aspell"))
          )
          ((executable-find "hunspell")
           (setq ispell-program-name "hunspell")
           (setq ispell-really-hunspell t)
           )
         )
  (ispell-dictionary "british-ize-w_accents")
  :config
  (defun my/flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  (unbind-key "C-." flyspell-mode-map)
  )



;;; auto-revert-mode:
;;  -----------------
(use-package autorevert
  :ensure nil
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  )


;;; dired:
;;  ------
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("^" . (lambda () (interactive) (find-alternate-file "..")))
              )
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  )


;;; magit:
;;  ------
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  ;; start a pinentry session for automatic signing of commits
  ;; -> requires allow-emacs-pinentry & allow-loopback-pinentry in gpg-agent.conf
  ;; see https://stackoverflow.com/q/60812866/
  (pinentry-start)
  (global-auto-revert-mode 1)
  (setq-default magit-git-environment
                (cons "PINENTRY_USER_DATA=USER_CURSES=0" magit-git-environment))
  :custom
  (magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
  )

;; (use-package forge
;;   :after magit)

;;; ediff:
;;  ------
(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  :hook
  (ediff-before-setup . my/store-pre-ediff-winconfig)
  (ediff-quit . my/restore-pre-ediff-winconfig)
  )

;; Restore window configuration after ediff:
;; Source: http://emacs.stackexchange.com/a/17089
(defvar my/ediff-last-windows nil)
(defun my/store-pre-ediff-winconfig ()
  "Store window configuration before ediff call."
  (setq my/ediff-last-windows (current-window-configuration)))
(defun my/restore-pre-ediff-winconfig ()
  "Restore saved window configuration after ediff ends."
  (set-window-configuration my/ediff-last-windows))


;;; tramp:
;;  ------
(defvar my/purmer-conda-path "/net/purmer/data1/debackere/miniconda3/bin/")
(defvar my/purmer-ms-path "/home/debackere/.emacs.d/.cache/lsp/mspyls/")
(use-package tramp
  :demand
  :config
  ;; need to add conda path for remote pyls server
  (add-to-list 'tramp-remote-path my/purmer-conda-path)
  (add-to-list 'tramp-remote-path my/purmer-ms-path)
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")))
  )


;;; wgrep:
;;  ------
(use-package wgrep-ag)

;;; projectile:
;;  ----
(use-package projectile
  :bind
  (:map projectile-mode-map
        ("s-p" . 'projectile-command-map)
        ()
        )
  :init
  (projectile-mode t)
  :custom
  (projectile-completion-system 'ivy)

  )

;;; ivy:
;;  ----
(use-package ivy
  :diminish (ivy-mode . "")
  :init
  (defun ivy-toggle-mark ()
    "Toggle mark for current candidate and move forwards."
    (interactive)
    (if (ivy--marked-p)
        (ivy-unmark)
      (ivy-mark)))
  :bind
  (:map ivy-minibuffer-map
        ("C-SPC" . ivy-toggle-mark))
  ("C-c C-o" . ivy-occur)
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (ivy-count-format "%d/%d ")
  ;; set actions when running C-x b
  ;; replace "frame" with window to open in new window
  (ivy-set-actions
   'ivy-switch-buffer
   '(("j" switch-to-buffer-other-frame "other frame")
     ("k" kill-buffer "kill")
     ("r" ivy--rename-buffer-action "rename")))
  )

(use-package counsel
  :bind
  (;; use swiper instead of isearch
   ;; if you press M-j, word at point is inserted
   ("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c C-t" . counsel-outline)
   ;; Still not completely happy with this one
   ;; not the same as helm-show-kill-ring
   ("M-y" . counsel-yank-pop)
   ;; If called with prefix argument, directory and args can be provided
   ("C-c s" . counsel-ag)
   )
  :custom
  ;; separate history items with line of dashes
  (counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n"))
  )

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))


;;; window movement:
;;  ----------------

;;;; winum
;; Navigate windows by number.

(use-package winum
  :custom
  (winum-keymap
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
     (define-key map (kbd "M-1") 'winum-select-window-1)
     (define-key map (kbd "M-2") 'winum-select-window-2)
     (define-key map (kbd "M-3") 'winum-select-window-3)
     (define-key map (kbd "M-4") 'winum-select-window-4)
     (define-key map (kbd "M-5") 'winum-select-window-5)
     (define-key map (kbd "M-6") 'winum-select-window-6)
     (define-key map (kbd "M-7") 'winum-select-window-7)
     (define-key map (kbd "M-8") 'winum-select-window-8)
     (define-key map (kbd "M-9") 'winum-select-window-9)
     map)
   )
  :custom-face
  (winum-face ((t (:weight bold
                   :foreground "red")))
              )
  :config
  (winum-mode 1)
  )


;;;; buffer-move
;; Move buffers between windows.

(use-package buffer-move
  :bind
  (("<C-M-S-up>" . 'buf-move-up)
   ("<C-M-S-down>" . 'buf-move-down)
   ("<C-M-S-left>" . 'buf-move-left)
   ("<C-M-S-right>" . 'buf-move-right))
  )


;;;; window splitting

(defun my/vsplit-other-window ()
  "Split window vertically and switch to that window."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))
(defun my/hsplit-other-window ()
  "Split window horizontally and switch to that window."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))
(defun my/toggle-window-split ()
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
(defun my/switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))


;;;; custom shortcuts
(bind-key "C-x 2" 'my/vsplit-other-window)
(bind-key "C-x 3" 'my/hsplit-other-window)
(bind-key "C-x |" 'toggle-window-split)
(bind-key "<C-S-down>" 'shrink-window)
(bind-key "<C-S-up>" 'enlarge-window)
(bind-key "<C-S-left>" 'shrink-window-horizontally)
(bind-key "<C-S-right>" 'enlarge-window-horizontally)
(bind-key "<f10>" 'my/switch-to-minibuffer-window)
;; the fullscreen of frame.el makes me lose my menu bar
(global-unset-key (kbd "<f11>"))


;;; buffer navigation:
;;  ------------------

(defun my/push-mark-no-activate ()
  "Push `point' to `mark-ring' and do not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is
disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun my/jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.

This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))

;; copied from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun my/smarter-move-beginning-of-line (arg)
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

;;;; shortcuts
(bind-key "C-`" 'my/push-mark-no-activate)
(bind-key "M-`" 'my/jump-to-mark)
(bind-key "C-a" 'my/smarter-move-beginning-of-line)

;;;; avy
(use-package avy
  :diminish t
  :bind ("C-." . avy-goto-char))

(use-package avy-zap
  :diminish t
  :bind ("M-z" . avy-zap-to-char-dwim))

;;;; function navigation
(bind-key "C-M-S-a" 'beginning-of-defun)
(bind-key "C-M-S-e" 'end-of-defun)


;;; Text manipulation:
;;  ------------------
(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

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
        (next-logical-line)
        ))

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

;;;; shortcuts
(bind-key "C-w" 'cut-line-or-region)
(bind-key "M-w" 'copy-line-or-region)
(bind-key "C-M-q" 'unfill-paragraph)
(bind-key "M-;" 'comment-or-uncomment-region-or-line)
(bind-key "M-RET" 'comment-indent-new-line)


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
  (mc/insert-numbers-default 1)
  )

;;;; visual-regexp
;; https://github.com/benma/visual-regexp-steroids.el/issues/21
;; to get visual-regexp-steroids to work:
;; change vr--command-python-default python -> python3, no unicode errors
;; need to run python3 for *Packages* error
(use-package visual-regexp-steroids
  :bind
  ("C-c r" . vr/replace)
  ("C-c q" . vr/query-replace)
  )


;;; Themes:
;;  -------
(defalias 'switch-theme 'counsel-load-theme)

;; # You may need to run these two lines if you haven't set up Homebrew
;; # Cask and its fonts formula.
;; brew install caskroom/cask/brew-cask
;; brew tap caskroom/fonts

;; brew cask install font-source-code-pro
(add-to-list 'default-frame-alist
             (cond
              ((string-equal system-type "darwin")    '(font . "Source Code Pro 14"))
              ((string-equal system-type "gnu/linux") '(font . "Source Code Pro 12"))))


;;;; Icons:
(use-package all-the-icons)
(use-package all-the-icons-dired)
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))



;;;; powerline:
(use-package powerline
  :disabled
  :config
  (powerline-default-theme))

;;;; Default theme:
(use-package material-theme)
(use-package dracula-theme)
(use-package solarized-theme)
(use-package gotham-theme
  :config
  (load-theme 'material t)
  )


;;; Languages:
;;  ----------
;;;; company-mode
;; Setup company-mode for autocompletion

(use-package company
  :diminish (company-mode . "")
  :hook
  (prog-mode . company-mode)
  :bind (:map company-active-map
              ;; from https://github.com/company-mode/company-mode/issues/246#issuecomment-65064467
              ;; pressing TAB twice results in autocompletion of the selected item
              ("TAB" . company-complete)
              ;; unbind return from completion
              ("<tab>")
              ("RET")
              ("<return>")
         )
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.05)
  (company-show-numbers t)
  )

(use-package company-box
  :diminish (company-box-mode . "")
  :hook (company-mode . company-box-mode))

;;;; Flycheck
(use-package flycheck
  :diminish (flycheck-mode . "")
  :init
  (global-flycheck-mode)
  )

;;;; lsp
(setq lsp-keymap-prefix "s-l")
;; recommendations in https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 64000000)

(use-package lsp-mode
  :diminish (yas-minor-mode . "")
  :hook
  (python-mode . lsp)
  ;; enable yas-minor-mode on lsp-mode to fix completion error
  (lsp-mode . yas-minor-mode)
  :commands lsp
  :config
  ;; configuration of remote python language server
  (lsp-register-client
   (make-lsp-client :new-connection
                    ;; (lsp-tramp-connection "Microsoft.Python.Language Server")
                    (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  :custom
  (lsp-prefer-capf t)
  (lsp-idle-delay 0.5)
  (lsp-enable-snippet nil)
  (lsp-modeline-code-actions-mode 1)
  ;; (lsp-auto-guess-root t)
  )

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-auto-install-server t)
  (setq lsp-python-ms-python-executable "python3")
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

;; ;; to make this work, run npm install -g pyright
;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-pyright)
;;                           (lsp))))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :bind ("M-i" . lsp-ivy-workspace-symbol-or-imenu)
  )

(defun lsp-ivy-workspace-symbol-or-imenu (arg)
  (interactive "P")
  (if lsp-mode
      (lsp-ivy-workspace-symbol arg)
    (counsel-imenu))
  )

(bind-key "C-c C-j" 'counsel-imenu)

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  ("s->" . lsp-ui-find-next-reference)
  ("s-<" . lsp-ui-find-prev-reference)
  :config
  (lsp-ui-peek-enable 1)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;;; LaTeX
;; LaTeX environment in Emacs. Work in progress.
(use-package reftex
  :defer t
  :diminish reftex-mode
  :custom
  (reftex-plug-into-auctex t)
  )

(use-package tex
  ;; to get working: https://github.com/jwiegley/use-package/issues/379
  :ensure auctex
  :defer t
  :config
  ;; latexmk document compilation
  ;; see http://tex.stackexchange.com/q/10561
  (add-to-list 'TeX-command-list '("LaTeX Make" "latexmk -pdf -f %t" TeX-run-TeX))
  (add-to-list 'TeX-command-list '("View" "open %s.pdf" TeX-run-command))
  (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "LaTeX Make")))

  ;; enable folding of environments
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (TeX-fold-mode 1)))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'TeX-after-compilation-finished-functions #'my/tex-close-TeX-buffer)

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

(defun my/tex-close-TeX-buffer (_output)
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

;;;; python
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  )

;;;; Lua
;; Requires lua and luarocks installations, available through Homebrew

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))


;;;; elisp
(use-package eldoc
  :diminish (eldoc-mode . "")
  )


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
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(custom-safe-themes
   '("afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" default))
 '(fci-rule-color "#ECEFF1")
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   '(yaml-mode ws-butler keychain-environment lsp-python-ms ag gotham-theme projectile lsp-ui lsp-ivy flycheck lsp-mode company solarized-theme tramp pinentry wgrep-ag visual-regexp-steroids super-save lua-mode all-the-icons-ivy-rich-mode all-the-icons-dired all-the-icons-ivy-rich powerline all-the-icons avy-zap smartparens latex auctex tex material-theme multiple-cursors buffer-move winum magit exec-path-from-shell diminish use-package))
 '(smartparens-global-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(winum-face ((t (:weight bold :foreground "red"))) t))
