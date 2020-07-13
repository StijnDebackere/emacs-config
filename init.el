;;; init.el --- Emacs init file

;; Maintainer: https://github.com/StijnDebackere

;;; Commentary:

;; Emacs init file pieced together by Stijn Debackere from
;; different places on the internet, usually source links are included
;; TODO
;; - look into epkg + borg setup to use github packages instead of elpa
;; - organize ivy switch buffer in open buffers and recently closed files

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
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)


;;;; Server start-up
;; Start the emacs server so files are opened in the opened emacs instance.
(use-package server
  :ensure t
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
  (setq mac-command-modifier nil)
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
  :ensure t
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


;;;; Backups
;; make back-ups to the ~/.emacs.d/backups directory
;; https://stackoverflow.com/q/151945/
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))

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
      auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t))
      auto-save-timeout 20              ; number of seconds idle time before
                                        ; auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves
                                        ; (default: 300)
      fill-column 100
      )


;;; smartparens:
;;  ------------
(use-package smartparens
  :ensure t
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
  :ensure t
  :diminish outline-minor-mode
  :config
  (use-package outline-magic
    :ensure t
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
;; (use-package flyspell
;;   :hook
;;   (text-mode . flyspell-mode)
;;   (org-mode . flyspell-mode)
;;   (TeX-mode . flyspell-mode)
;;   (prog-mode . flyspell-prog-mode)
;;   (emacs-lisp-mode . flyspell-prog-mode)
;;   (python-mode . flyspell-prog-mode)
;;   :custom
;;   ;; (setenv "LANG" "en_GB.UTF-8")
;;   (ispell-program-name "hunspell")
;;   (ispell-really-hunspell t)
;;   (ispell-dictionary "en_GB")
;;   :init
;;   )

;; (defun my/flyspell-check-next-highlighted-word ()
;;   "Custom function to spell check next highlighted word."
;;   (interactive)
;;   (flyspell-goto-next-error)
;;   (ispell-word))


;;; auto-revert-mode:
;;  -----------------
(use-package autorevert
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
  :ensure t
  :bind
  ("C-x g" . magit-status)
  :config
  (global-auto-revert-mode 1)
  :custom
  (magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
  )


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
(use-package tramp
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")))
  )


;;; ivy:
;;  ----
;;;; TODO
;; - Better shortcut for counsel-outline
;; - 
;; - Shortcut for ivy-actions
;; - ivy-ag workflow with counsel-occur
(use-package ivy
  :ensure t
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
  :ensure t
  :bind
  (;; use swiper instead of isearch
   ;; if you press M-j, word at point is inserted
   ("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-i" . counsel-imenu)
   ("C-c C-o" . counsel-outline)
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


;;; window movement:
;;  ----------------

;;;; winum
;; Navigate windows by number.

(use-package winum
  :ensure t
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
  :ensure t
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
;; (global-unset-key (kbd "C-."))
(use-package avy
  :diminish t
  :ensure t
  :bind ("C-." . avy-goto-char))

(use-package avy-zap
  :diminish t
  :ensure t
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
  :ensure t
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
;; (use-package visual-regexp-steroids
;;   :ensure t
;;   :bind
;;   ("C-c r" . vr/replace)
;;   ("C-c q" . vr/query-replace)
;;   ("C-s" . vr/isearch-forward)
;;   ("C-r" . vr/isearch-backward)
;;   :custom
;;   (vr/command-python
;;    "python3 ~/.emacs.d/elpa/visual-regexp-steroids-20170222.253/regexp.py")
;;   )


;;; Themes:
;;  -------
(defun switch-theme (theme)
  "Disable any currently active themes and load THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disable any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))


;; # You may need to run these two lines if you haven't set up Homebrew
;; # Cask and its fonts formula.
;; brew install caskroom/cask/brew-cask
;; brew tap caskroom/fonts

;; brew cask install font-source-code-pro
(add-to-list 'default-frame-alist
             (cond
              ((string-equal system-type "darwin")    '(font . "Source Code Pro 14"))
              ((string-equal system-type "gnu/linux") '(font . "Source Code Pro 12"))))

;;;; Default theme:
(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  )



;;; Languages:
;;  ----------

;;;; company-mode
;; Setup company-mode for autocompletion

;;;;; TODO:
;; - company-quickhelp uses native mac system for tooltips, get very
;;   long and unintelligible. Should use gtk version instead of cocoa?
;; - documentation for some of my code does not show up correctly

(use-package company
  :ensure t
  :diminish (company-mode . "")
  :hook
  (prog-mode . company-mode)
  :bind (:map company-active-map
         ("C-c d" . my/company-show-doc-buffer)
         )
  :config
  ;; disable company-quickhelp until I figure out how to solve TODO
  (use-package company-quickhelp
    :disabled t
    :config (company-quickhelp-mode 0)
    )
  (use-package company-auctex
    :ensure t)
  (require 'company-auctex)
  (company-auctex-init)
  (add-to-list 'company-backends '(company-elisp
                                   company-anaconda
                                   company-auctex
                                   company-lua
                                   company-math-symbols-unicode)
               )
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  (company-show-numbers t)
  )

;; function to show documentation for functions
(defun my/company-show-doc-buffer ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))


;;;; Flycheck
;; Syntax checking
(use-package flycheck
  :ensure t
  :custom
  (flycheck-python-flake8-executable "python3")
  (flycheck-python-pycompile-executable "python3")
  (flycheck-python-pylint-executable "python3")
  (flycheck-html-tidy-executable "tidy")
  :config
  (global-flycheck-mode)
)

;;;; Python
;; Make Emacs into a nice IDE for python development
(use-package anaconda-mode
  :ensure t
  :diminish (anaconda-mode . "")
  :hook (python-mode . anaconda-mode))

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
  :defer t
  :ensure auctex
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
  :custom
  ;; parse tex files on load and save
  (TeX-auto-save t)
  (TeX-parse-self t)
  (preview-auto-cache-preamble 1)
  ;; do not change font height or width in latex files
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 'color)
  )

;; Close tex-output buffer if there are only warnings after compilation
;; see https://emacs.stackexchange.com/q/38258/
(defcustom TeX-buf-close-at-warnings-only t
  "Close TeX buffer if there are only warnings."
  :group 'TeX-output
  :type 'boolean)

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

(add-hook 'TeX-after-compilation-finished-functions #'my-tex-close-TeX-buffer)


;;;; Lua
;; Requires lua and luarocks installations, available through Homebrew

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))


;;;; elisp
(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil)
  )

;;; Custom:
;;  -------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy-zap company-auctex smartparens latex auctex tex material-theme anaconda-mode flycheck company multiple-cursors buffer-move winum magit exec-path-from-shell diminish use-package))
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(winum-face ((t (:weight bold :foreground "red"))) t))
