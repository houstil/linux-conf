;;;;;;;;;;;;;;;;;;;;;;;
;; Services - Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; start the emacs server
;; (require 'server)
;; (setq server-host (system-name)
;;       server-use-tcp t)

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

(require 'mail-conf)

(require 'coding-conf)

(require 'orgmode-conf)

(require 'versiondiff-conf)

(require 'apparance-conf)

;; tramp authentification
(require 'tramp)
(setq tramp-default-method "ssh")

;; use mingus to pilot my mpd
(add-to-list 'load-path "~/.emacs.d/mingus")
(require 'mingus)




