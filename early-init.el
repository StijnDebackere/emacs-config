;;; early-init.el --- Early initialization  -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before init.el and before package.el would otherwise auto-activate
;; every package sitting in `~/.emacs.d/elpa'; straight.el is now the only
;; thing managing packages, so that has to happen here rather than in
;; init.el.

;;; Code:

(setq package-enable-at-startup nil)

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(add-to-list 'default-frame-alist '(undecorated-round . t))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; early-init.el ends here
