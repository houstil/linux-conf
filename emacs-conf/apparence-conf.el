;; winner mode to easily manage windows configuration
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; elscreen - screen like windows management in emacs
;; (pacmans-cload 'elscreen "elscreen" 
;;                '(lambda ()
;;                   (add-to-list 'load-path "~/.emacs.d/elscreen-1.4.6")
;;                   (setq elscreen-prefix-key "@")
;;                   ;; to be able to use the @ key in term mode :
;;                   (add-hook 'term-mode-hook 
;;                             '(lambda ()
;;                                (when (current-local-map)
;;                                  (use-local-map (copy-keymap (current-local-map)))
;;                                  (local-set-key (kbd "@ a") '(lambda () (interactive)(term-send-raw-string "@"))))
;;                                ))
;;                   (load "elscreen" "ElScreen" t)
;;                   ))

;; minimal fringe
(set-fringe-mode 3)

;; non-blinking cursor
(blink-cursor-mode 0)

;; inhibit sartup screen
(custom-set-variables
 '(comint-completion-addsuffix t)
 '(comint-completion-autolist t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t))

;; to show colored-matching parren
(pacmans-cload 'highlight-parentheses
               '(lambda () (highlight-parentheses-mode t)))

(defun hide-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; align using space, not tabs
(setq align-default-spacing 1)

;;;;;;;;;;;;;;;
;; Powerline ;;
;;;;;;;;;;;;;;;

(pacmans-cload 'powerline
               '(lambda ()
                  (powerline-default-theme)))

;;;;;;;;;;;;;;;;;;
;; color themes ;;
;;;;;;;;;;;;;;;;;;

(if (window-system)
    (progn 
      (require 'color-theme)
      (add-hook 'after-init-hook '(lambda () (color-theme-solarized-dark))))
  (load-theme 'cyberpunk t)
  )

;; at last we toggle full screen on
(add-hook 'after-init-hook
          '(lambda ()
             (if (eq system-type 'windows-nt)
                 (w32-send-sys-command #xf030)
               )
             ))

(provide 'apparence-conf)
