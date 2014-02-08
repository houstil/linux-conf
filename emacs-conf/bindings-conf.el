(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun exchange-point-and-mark-nomark ()
  "Exchange the poind and the mark without marking the region between them."
  (interactive)
  (exchange-point-and-mark t))


;; load ErgoEmacs keybinding
(pacmans-cload
 'ergoemacs-mode
 '(lambda ()
    (progn
      ;; turn on minor mode ergoemacs-mode
      (setq ergoemacs-mode-used "5.7.5")
      (setq ergoemacs-theme     "5.7.5")

      ;; choose the us layout
      (setq ergoemacs-keyboard-layout "colemak")
      (ergoemacs-mode 1)
      (setq saved-overriding-map nil)

      ;; ergoemacs corrections
      (add-hook 'minibuffer-setup-hook
        	'(lambda ()
        	   (ergoemacs-local-unset-key (kbd "M-n"))
        	   (ergoemacs-local-unset-key (kbd "M-i"))
        	   (ergoemacs-local-unset-key (kbd "C-f"))
        	   ))

      (add-hook 'comint-mode-hook
                '(lambda ()
                   (ergoemacs-local-set-key (kbd "M-u") 'comint-previous-input)
                   (ergoemacs-local-set-key (kbd "M-e") 'comint-next-input)
                   ))
      
      (global-set-key (kbd "C-S-f")    'rename-buffer)
      (global-set-key (kbd "C-f")      'ido-switch-buffer)

      ;; rework a few movements
      (ergoemacs-key "M-L" 'end-of-line         "end-of-line"        )
      (ergoemacs-key "M-J" 'beginning-of-line   "beginning-of-line"  )
      (ergoemacs-key "M-H" 'end-of-buffer       "end-of-buffer"      )
      (ergoemacs-key "M-h" 'beginning-of-buffer "beginning-of-buffer")
      (ergoemacs-key "M-m" 'exchange-point-and-mark-nomark "exchange point and mark")
      (ergoemacs-key "M--" 'iconify-or-deiconify-frame "minimise emacs window")
      
      ;; frame movement
      (global-set-key (kbd "s-i") 'windmove-up)
      (global-set-key (kbd "s-k") 'windmove-down)
      (global-set-key (kbd "s-j") 'windmove-left)
      (global-set-key (kbd "s-l") 'windmove-right)

      ;; disabling this boring print command
      (global-unset-key (kbd "C-p"))

      ;; (ergoemacs-key "RET" 'newline-and-indent "New line")
      (ergoemacs-key "M-;" 'vi-open-line-below "New line below")
      (ergoemacs-key "M-S-;" 'vi-open-line-above "New line above"))
    )
 )

;; no hilights back to point command
;; (global-set-key (kbd "s-b") '(lambda () (interactive) (exchange-point-and-mark t)))

;; comment and uncomment bindings
(global-set-key (kbd "C-M-\"") 'comment-box)
(global-set-key (kbd "M-\"") 'comment-or-uncomment-region)

;; buffer switching bindings
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-<tab>")  'switch-to-previous-buffer)
;; (global-set-key (kbd "S-<f6>")   'ibuffer)
(global-set-key (kbd "<f6>")     'ido-switch-buffer)


;; term buffers bindings
;; (global-set-key (kbd "s-t")      'visit-ansi-term)
;; (global-set-key (kbd "s-T")      'multi-term-restart)

;; set f7 to query-replace and S-f7 to replace-string
(global-set-key (kbd "S-<f7>") 'query-replace)
(global-set-key (kbd "<f7>") 'replace-string)
;; (global-set-key (kbd "s-<f7>") 'replace-regexp)
;; (global-set-key (kbd "s-S-<f7>") 'find-grep)
(setq grep-find-command
  "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -i -n -e -I")

; to allow seemless edition in terminal mode
(global-set-key (kbd "C-@")  'set-mark-command)

;; set s-w to server-edit (to exit a emacsclient session)
;; (add-hook 'server-switch-hook
;;           (lambda ()
;;             (when (current-local-map)
;;               (use-local-map (copy-keymap (current-local-map))))
;;              (local-set-key (kbd "s-w") 'server-edit)))

(provide 'bindings-conf)
