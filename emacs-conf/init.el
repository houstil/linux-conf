;;;;;;;;;;;;;;;;;;;;;;;
;; Services - Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set the load-path
(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sub Modules Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'pacmans-conf)

(require 'bindings-conf)

(require 'edit-conf)

(require 'aliases-conf)

(require 'files-conf)

(require 'myterm-conf)

(require 'userint-conf)

(require 'connection-conf)

(require 'coding-conf)

(require 'orgmode-conf)

(require 'versiondiff-conf)

(require 'apparance-conf)

