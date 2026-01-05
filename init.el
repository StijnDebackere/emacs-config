;;; init.el --- Emacs init file

;; Maintainer: https://github.com/StijnDebackere

;;; Commentary:

;; Emacs init file pieced together from different places on the
;; internet, usually source links are included

;;; Code:

;;; Startup:
;;  --------

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

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

(straight-use-package 'use-package)
(straight-use-package 'diminish)

;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib . "org"))))


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
    (if (not (server-running-p)) (server-start))))

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
  (put 'suspend-frame 'disabled t))

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

;;;;; path loading
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

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

;;; automatic whitespace removal
(use-package ws-butler
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))


;;; save emacs buffer upon loss of focus
(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode +1))


;;; smartparens
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
  (sp-show-pair-from-inside nil))


;;; outline-magic
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
                                   "\\subparagraph")))))))


(use-package minions
  :config
  (minions-mode 1))

;;;; Text
;; ;; Disabled because it annoys me in COMMIT_MSG and yml files...
;; (defun sdb/enable-dead-keys ()
;;   "Enable dead key expansion with TeX input method in text mode."
;;   (activate-input-method "TeX"))
;; (add-hook 'text-mode-hook 'sdb/enable-dead-keys)

;; ;;; flyspell:
;; ;; setup flyspell
;; ;;;; TODO
;; ;; - ispell-hunspell-dict-paths-alist contains right dictionaries, but
;; ;;   ispell-hunspell-dictionary-alist does not seem to load them
;; ;; - check https://facility9.com/2015/01/unsetting-key-bindings-in-emacs/
;; (use-package flyspell
;;   :diminish (flyspell-mode . "")
;;   :hook
;;   (text-mode . flyspell-mode)
;;   (org-mode . flyspell-mode)
;;   (TeX-mode . flyspell-mode)
;;   (prog-mode . flyspell-prog-mode)
;;   (emacs-lisp-mode . flyspell-prog-mode)
;;   (python-mode . flyspell-prog-mode)
;;   :bind
;;   ("C-c w" . sdb/flyspell-check-next-highlighted-word)
;;   ("C-c b" . flyspell-buffer)
;;   :custom
;;   ;; (setenv "LANG" "en_GB.UTF-8")
;;   (progn (cond
;;           ((executable-find "aspell")
;;            ;; you may also need `ispell-extra-args'
;;            (setq ispell-program-name "aspell"))
;;           )
;;           ((executable-find "hunspell")
;;            (setq ispell-program-name "hunspell")
;;            (setq ispell-really-hunspell t)
;;            )
;;          )
;;   (ispell-dictionary "british-ize-w_accents")
;;   :config
;;   (defun sdb/flyspell-check-next-highlighted-word ()
;;     "Custom function to spell check next highlighted word."
;;     (interactive)
;;     (flyspell-goto-next-error)
;;     (ispell-word))
;;   (unbind-key "C-." flyspell-mode-map)
;;   )


;;; auto-revert-mode
;; autorevert buffer upon file changes
(use-package autorevert
  :ensure nil
  :delight auto-revert-mode
  :config
  (global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))


;;; dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("<backspace>" . dired-up-directory)
              ("b" . dired-up-directory)
              ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top))


;;; magit
;;;; TODO:
;; - get pinentry working
(use-package magit
  :bind
  ("C-x g" . magit-status)
  :after (pinentry)
  :config
  ;; start a pinentry session for automatic signing of commits
  ;; -> requires allow-emacs-pinentry & allow-loopback-pinentry in gpg-agent.conf
  ;; see https://stackoverflow.com/q/60812866/
  (pinentry-start)
  (global-auto-revert-mode 1)
  (setq-default magit-git-environment
                (cons "PINENTRY_USER_DATA=USER_CURSES=0" magit-git-environment))
  :custom
  (magit-log-arguments (quote ("--decorate" "-n256")))
  (magit-refresh-status-buffer nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))


;;; ediff
(use-package ediff
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
  :demand
  :config
  :custom
  (tramp-default-method "ssh")
  (tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config"))))


;;; ripgrep
(use-package rg)

;;; wgrep
(use-package wgrep-ag)

;;; projectile:
(use-package projectile
  :bind
  (:map projectile-mode-map
        ("s-p" . 'projectile-command-map))
  :init
  (projectile-mode t)
  :custom
  (projectile-completion-system 'ivy))


;;; ivy
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
     ("r" ivy--rename-buffer-action "rename"))))

;;; counsel
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
   ("C-c s" . counsel-ag))
  :custom
  ;; separate history items with line of dashes
  (counsel-yank-pop-separator (concat "\n" (make-string 70 ?-) "\n")))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))


;;; window movement

;;;; ace-window
(use-package ace-window
  :ensure t
  :defer 1
  :bind
  ("M-o" . ace-window)
  :config
  ;; see https://github.com/abo-abo/ace-window/issues/44#issuecomment-264923922
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :background nil
                      :weight 'bold
                      :height 10.0)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "indian red")
  (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always nil
        aw-dispatch-alist
        '((?x aw-delete-window     "Ace - Delete Window")
          (?c aw-swap-window       "Ace - Swap Window")
          (?n aw-flip-window)
          (?h aw-split-window-vert "Ace - Split Vert Window")
          (?v aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u winner-undo)
          (?r winner-redo)))

  ;; (when (package-installed-p 'hydra)
  ;;   (defhydra hydra-window (:color blue :hint nil :idle 0.4 :timeout 3)
  ;;     "
  ;;                                                                             ╭────────────┐
  ;;                                                                             │ Ace Window │
  ;;           ╭─────────────────────────────────────────────────────────────────┴────────────╯
  ;;               [^w^] windows size [^r^] winner redo  [^u^] winner undo  [^o^] scroll other
  ;;               [^a^] jump window  [^s^] jump window  [^d^] jump window  [^f^] jump window
  ;;               [^g^] max. current [^h^] split horz.  [^j^] jump window  [^k^] jump window
  ;;               [^l^] jump window  [^;^] swap window  [^x^] del. window  [^c^] swap window
  ;;               [^v^] split vert.  [^b^] balance win. [^n^] last window  [^m^] max window
  ;;           --------------------------------------------------------------------------------
  ;;              ")
  ;;   (defhydra hydra-window-size (:color red)
  ;;     "Windows size"
  ;;     ("j" shrink-window-horizontally "shrink horizontal")
  ;;     ("k" shrink-window "shrink vertical")
  ;;     ("l" enlarge-window "enlarge vertical")
  ;;     (";" enlarge-window-horizontally "enlarge horizontal"))
  ;;   (defhydra hydra-window-frame (:color red)
  ;;     "Frame"
  ;;     ("f" make-frame "new frame")
  ;;     ("x" delete-frame "delete frame"))
  ;;   (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
  ;;   (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
  ;;   (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
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


;;; buffer navigation
(defun sdb/push-mark-no-activate ()
  "Push `point' to `mark-ring' and do not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is
disabled."
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun sdb/jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.

This is the same as using \\[set-mark-command] with the prefix
argument."
  (interactive)
  (set-mark-command 1))

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

;;;; shortcuts
(bind-key "C-`" 'sdb/push-mark-no-activate)
(bind-key "M-`" 'sdb/jump-to-mark)
(bind-key "C-a" 'sdb/smarter-move-beginning-of-line)

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


;;; Text manipulation
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

;;;; shortcuts
(bind-key "C-w" 'cut-line-or-region)
(bind-key "M-w" 'copy-line-or-region)
(bind-key "C-M-q" 'unfill-paragraph)
(bind-key "M-;" 'comment-or-uncomment-region-or-line)
(bind-key "M-RET" 'comment-indent-new-line)

;;;; combobulate
(use-package treesit
  :ensure nil
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
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  ;; (use-package combobulate
  ;;   ;; Optional, but recommended.
  ;;   ;;
  ;;   ;; You can manually enable Combobulate with `M-x
  ;;   ;; combobulate-mode'.
  ;;   :bind
  ;;   ("C-c o o" . combobulate)
  ;;   :hook ((python-ts-mode . combobulate-mode)
  ;;          (js-ts-mode . combobulate-mode)
  ;;          (css-ts-mode . combobulate-mode)
  ;;          (yaml-ts-mode . combobulate-mode)
  ;;          (typescript-ts-mode . combobulate-mode)
  ;;          (tsx-ts-mode . combobulate-mode))
  ;;   ;; Amend this to the directory where you keep Combobulate's source
  ;;   ;; code.  ;;   :load-path ("~/Repositories/combobulate/")))
  )

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


;;; Themes
(defalias 'switch-theme 'counsel-load-theme)

;; # You may need to run these two lines if you haven't set up Homebrew
;; # Cask and its fonts formula.
;; brew install caskroom/cask/brew-cask
;; brew tap caskroom/fonts

;; brew cask install font-source-code-pro
(add-to-list 'default-frame-alist
             (cond
              ((string-equal system-type "darwin")    '(font . "Iosevka Comfy 16"))
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
(use-package all-the-icons)
(use-package all-the-icons-dired)
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

;;;; powerline
(use-package powerline
  :disabled
  :config
  (powerline-default-theme))

;;;; Default theme
(use-package material-theme)
(use-package dracula-theme)
(use-package solarized-theme)
(use-package gotham-theme
  :config
  (load-theme 'material t))


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
              ("<return>"))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.05)
  (company-show-numbers t))

(use-package company-box
  :diminish (company-box-mode . "")
  :hook (company-mode . company-box-mode))

;;;; GitHub CoPilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :diminish (copilot-mode . "")
  :bind (:map copilot-completion-map
              ("TAB" . copilot-accept-completion)
              )
  )

;;;; Flycheck
(use-package flycheck
  :diminish (flycheck-mode . "")
  :init
  (global-flycheck-mode))

;; ;;;; SQL
;; (use-package flymake-sqlfluff
;;   :hook (sql-mode . #'flymake-sqlfluff-load))

;;;; python
;; add functionality to automatically load the correct python venv on
;; focus changes -> not required, lsp-pyright with
;; lsp-pyright-multi-root nil loads local venv correctly
;; (use-package pyvenv
;;   :ensure t)

;; (use-package pyvenv-auto
;;   :ensure t)

;; (defun switch-python-venv (&optional frame)
;;   "Switch Python virtualenv for FRAME."
;;   (when (eq major-mode 'python-mode)
;;     (pyvenv-auto-run)
;;   ))

;; (add-to-list 'window-selection-change-functions 'switch-python-venv)

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
   (prog-mode . lsp-mode)
   (sh-mode . lsp-mode)
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
  (lsp-prefer-capf t)
  (lsp-idle-delay 0.0)
  (lsp-enable-snippet nil)
  (lsp-modeline-code-actions-mode 1)
  (lsp-auto-execute-action nil)
  ;; (lsp-auto-guess-root t)
  )

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


;; ;; to use LanguageTool for spelling and grammar checking
;; (use-package lsp-ltex
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'lsp-ltex)
;;                        (lsp-deferred)))  ; or lsp-deferred
;;   :bind
;;   ("C-c l" . lsp-ltex-change-language)
;;   :init
;;   (setq lsp-ltex-version "15.2.0")  ; make sure you have set this, see below
;;   (setq lsp-ltex-check-frequency "save")
;;   (defun lsp-ltex-change-language ()
;;     "Change the LanguageTool language."
;;     (interactive)
;;     (let* ((completions '(("English" . "en-US")
;;                           ("Spanish" . "es")
;;                           ;; ("Automatic" . "auto")
;;                           ("Dutch" . "nl-BE")))
;;            (ltex-lang (cdr (assoc (ivy-completing-read "Select language for LanguageTool: " completions)
;;                                   completions))))
;;       (setq lsp-ltex-language ltex-lang)
;;       (message "Changed LanguageTool language to %s" ltex-lang)
;;       (lsp-restart-workspace))))

(defun lsp-ivy-workspace-symbol-or-imenu (arg)
  "Use counsel-imenu on ARG only if no lsp-mode available."
  (interactive "P")
  (if lsp-mode
      (lsp-ivy-workspace-symbol arg)
    (counsel-imenu)))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :bind ("M-i" . lsp-ivy-workspace-symbol-or-imenu))

(bind-key "C-c C-j" 'counsel-imenu)

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  ("s->" . lsp-ui-find-next-reference)
  ("s-<" . lsp-ui-find-prev-reference)
  ("C-c C-d" . lsp-ui-doc-glance)  ;; default ("s-l h g")
  :custom
  (lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;;; LaTeX
;; LaTeX environment in Emacs. Work in progress.
(use-package reftex
  :defer t
  :diminish reftex-mode
  :custom
  (reftex-plug-into-auctex t))
(use-package tex
  ;; to get working: https://github.com/jwiegley/use-package/issues/379
  :ensure auctex
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
(use-package eldoc
  :diminish (eldoc-mode . ""))

;; ;;;; SQL
;; (use-package sql
;;   :hook (sql-mode . lsp-mode)
;;   :custom
;;   (setq lsp-sqls-workspace-config-path nil)
;;   (setq lsp-sqls-connections
;;         '(((driver . "postgresql") (dataSourceName . "host=live-main.c56d9jndqmv2.us-east-2.redshift.amazonaws.com port=5439 user=tf_stijn_debackere_an password=local dbname=klarprod sslmode=disable"))
;;           ))
;;   )

;; (load "~/.emacs.d/sdb-init.el")


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
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(copilot-indent-offset-warning-disable t)
 '(custom-safe-themes
   '("afd761c9b0f52ac19764b99d7a4d871fc329f7392dfc6cd29710e8209c691477" default))
 '(fci-rule-color "#ECEFF1")
 '(flycheck-checker-error-threshold 1000)
 '(hl-sexp-background-color "#efebe9")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#B71C1C") (40 . "#FF5722") (60 . "#FFA000") (80 . "#558b2f") (100 . "#00796b")
     (120 . "#2196f3") (140 . "#4527A0") (160 . "#B71C1C") (180 . "#FF5722") (200 . "#FFA000")
     (220 . "#558b2f") (240 . "#00796b") (260 . "#2196f3") (280 . "#4527A0") (300 . "#B71C1C")
     (320 . "#FF5722") (340 . "#FFA000") (360 . "#558b2f")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vc-annotate-color-map '((20 . "#B71C1C") (40 . "#FF5722") (60 . "#FFA000") (80 . "#558b2f") (100 . "#00796b") (120 . "#2196f3") (140 . "#4527A0") (160 . "#B71C1C") (180 . "#FF5722") (200 . "#FFA000") (220 . "#558b2f") (240 . "#00796b") (260 . "#2196f3") (280 . "#4527A0") (300 . "#B71C1C") (320 . "#FF5722") (340 . "#FFA000") (360 . "#558b2f"))))
