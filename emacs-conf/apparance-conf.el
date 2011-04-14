;; to see a mini version of the current buffer :
;; (require 'minimap)

;; describe colors under the cursor
;;(require 'eyedropper)

;; winner mode to easily manage windows configuration
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; elscreen - screen like windows management in emacs
(add-to-list 'load-path "~/.emacs.d/elscreen-1.4.6")
(setq elscreen-prefix-key "@")
;; to be able to use the @ key in term mode :
(add-hook 'term-mode-hook 
	  '(lambda ()
	     (when (current-local-map)
	       (use-local-map (copy-keymap (current-local-map)))
	       (local-set-key (kbd "@ a") '(lambda () (interactive)(term-send-raw-string "@"))))
	     ))
(load "elscreen" "ElScreen" t)

;; To show corresponding paren
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)

;; minimal fringe
(set-fringe-mode 3)

;; set default size
(set-face-attribute 'default nil :height 120)

;; emacs desktop to save multiple emacs windows configuration
;; (setq desktop-dirname "~/.emacs.d/desktop")
;; (desktop-save-mode 1)

;; non-blinking cursor
(blink-cursor-mode 0)

;; inhibit sartup screen
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(inhibit-startup-screen t))

;; Prevent the annoying beep on errors
(setq visible-bell t)

;; hide if-def
;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (hide-ifdef-mode t)
;;            )
;; )

;; (setq hide-ifdef-initially t)


;; unicad autodetect a file coding system
(require 'unicad)
(unicad) ;; enable it

;; to show useless whitespaces
;; (require 'show-wspace) ;

;; to show colored-matching parren
(require 'highlight-parentheses)
(highlight-parentheses-mode t)

;; to show parentheses matching line in minibuffer
(require 'mic-paren) ; loading
(paren-activate)     ; activating

(defun hide-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; align using space, not tabs
(setq align-default-spacing 1)

;; to easily toggle the way emacs display long lines
(global-set-key (kbd "S-<f8>") 'toggle-truncate-lines)

;;;;;;;;;;;;;;;;;;;;;
;; apparance hooks ;;
;;;;;;;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (paren-toggle-matching-quoted-paren 1)
                      (paren-toggle-matching-paired-delimiter 1)
                      (highlight-parentheses-mode t)
)))

;;;;;;;;;;;;;;;;;;
;; color themes ;;
;;;;;;;;;;;;;;;;;;

;; additionnals color themes
(require 'color-theme)

;; zenbern is a low-contrast eye-friendly theme
(require 'zenburn)
(color-theme-zenburn)

;; (color-theme-comidia)


(provide 'apparance-conf)

