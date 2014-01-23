;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (window-system)
  ;; so that git doesn't slow emacs down too much
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (setq vc-handled-backends nil)
  ;; avoiding a snaily emacs
  (setq w32-get-true-file-attributes nil)
)

(add-to-list 'load-path "~/.emacs.d/")
;; to get a full list of emacs startups loads
(setq message-log-max t)
(cd "~")
;; to get more info on errors
(setq debug-on-error nil)

;; to put our custom configuration out of the init file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Apparences Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
;; (set-frame-font "Anonymous Pro-10")
(set-face-attribute 'default nil :height 80)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration Files Loading ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pacmans-conf)

(require 'package-list)

(require 'files-conf)

(require 'bindings-conf)

(require 'userint-conf)

(require 'edit-conf)

(require 'myterm-conf)

(require 'aliases-conf)

(require 'coding-conf)

(require 'functionnality-conf)

(require 'apparence-conf)
