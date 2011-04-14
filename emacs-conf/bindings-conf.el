;; load ErgoEmacs keybinding
(load "~/.emacs.d/ergoemacs-keybindings-5.1/ergoemacs-mode")

;; ergoemacs corrections
(add-hook 'minibuffer-setup-hook
          '(lambda ()
	     (ergoemacs-local-unset-key (kbd "M-i"))
	     (ergoemacs-local-unset-key (kbd "M-k"))
	     ))

(define-key minibuffer-local-map (kbd "M-i")    'previous-history-element)
(define-key minibuffer-local-map (kbd "M-k")    'next-history-element)

(global-set-key (kbd "s-:") 'search-backward-regexp)
(global-set-key (kbd "s-;") 'search-forward-regexp)

;; frame movement
(global-set-key (kbd "s-i") 'windmove-up)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)

;; turn on minor mode ergoemacs-mode
(ergoemacs-mode 1)

;; no hilights back to point command
(global-set-key (kbd "s-b") '(lambda () (interactive) (exchange-point-and-mark t)))

;; comment and uncomment bindings
(global-set-key (kbd "M-4") 'comment-box)
(global-set-key (kbd "s-'") 'comment-or-uncomment-region)

;; disabling this boring print command
(global-unset-key (kbd "C-p"))

;; buffer switching bindings
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-<tab>")  'switch-to-previous-buffer)
(global-set-key (kbd "S-<f6>")   'ibuffer)
(global-set-key (kbd "<f6>")     'ido-switch-buffer)
(global-set-key (kbd "M-n")      'ido-switch-buffer)
(global-set-key (kbd "M-N")      'rename-buffer)

;; term buffers bindings
(global-set-key (kbd "s-t")      'visit-ansi-term)
(global-set-key (kbd "s-T")      'multi-term-restart)

;; set f12 to dired
(load "~/.emacs.d/dired+.el")
(toggle-dired-find-file-reuse-dir 1)
(global-set-key (kbd "s-<f12>") 'dired)

;; set f7 to query-replace and S-f7 to replace-string
(global-set-key (kbd "S-<f7>") 'query-replace)
(global-set-key (kbd "<f7>") 'replace-string)
(global-set-key (kbd "s-<f7>") 'replace-regexp)
(global-set-key (kbd "s-S-<f7>") 'find-grep)
(setq grep-find-command
  "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -i -n -e -I")

;; to easily define macros
(global-set-key (kbd "M-<f3>") 'kmacro-start-macro)
(global-set-key (kbd "S-<f3>") 'kmacro-end-macro)
(global-set-key (kbd "<f3>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "s-<f3>") 'kmacro-insert-counter)
(global-set-key (kbd "s-S-<f3>") 'kmacro-set-counter)

;; to allow seemless edition in terminal mode
(global-set-key (kbd "C-@")  'set-mark-command)

;; some usefull toggeling
(global-set-key (kbd "<f4>") 'insert-register)
(global-set-key (kbd "<S-f4>") 'copy-to-register)

;; quick access to terminals
(global-set-key (kbd "<f12>") 'visit-ansi-term)
(global-set-key (kbd "S-<f12>") 'ansi-term)

;; set s-w to server-edit (to exit a emacsclient session)
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
             (local-set-key (kbd "s-w") 'server-edit)))

(provide 'bindings-conf)