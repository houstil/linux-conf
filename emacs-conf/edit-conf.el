
;; to comment a line without selecting it
(defadvice comment-or-uncomment-region (before slick-copy activate compile)
  "When called interactively with no active region, comment or uncomment a single line instead."
  (interactive (if mark-active
		   (list (region-beginning) (region-end)) (message  "Co|Unco line")
		   (list (line-beginning-position) (line-beginning-position  2)))))

;; to copy a line without selecting it
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;; enable the use of the x clipboard
(setq x-select-enable-clipboard t)

;; require smart tab : expand or indent on tab hit
(pacmans-cload
 'smart-tab
 "smart-tab"
 '(lambda () (progn
	      ;; smart-tab use hippie expand :
	      (setq hippie-expand-try-functions-list '(yas/hippie-try-expand
						       ;; senator-try-expand-semantic
						       try-expand-dabbrev
						       try-expand-dabbrev-all-buffers
						       try-expand-dabbrev-from-kill
						       try-complete-file-name-partially
						       try-complete-file-name
						       try-expand-all-abbrevs
						       try-expand-list
						       try-expand-line
						       try-complete-lisp-symbol-partially
						       try-complete-lisp-symbol
						       ))



	      ;; special cases for tab
	      (add-hook 'compilation-mode-hook
			'(lambda ()
			   (when (current-local-map)
			     (use-local-map (copy-keymap (current-local-map))))
			   (local-set-key (kbd "<tab>") 'compilation-next-error)
			   )))) 
 '(lambda () (el-get-install "smart-tab"))
)

;; forbid dabbrev to change case
(setq dabbrev-case-replace nil)

;; use undo-tree
(pacmans-cload
 'undo-tree
 "undo-tree"
  '(lambda () (el-get-install "undo-tree"))
  nil
)

;;;;;;;;;;;;;;;;;;;;;
;; Text Navigation ;;
;;;;;;;;;;;;;;;;;;;;;

;; to quickly jump to last change :
(pacmans-cload
 'goto-chg
 "goto-chg"
 '(lambda () (progn
     (global-set-key (kbd "s-h") 'goto-last-change)
     (global-set-key (kbd "s-H") 'goto-last-change-reverse)
     ))
 '(lambda () (auto-install-from-url "http://www.emacswiki.org/emacs/download/goto-chg.el"))
 )


;; define an additionnal function to open file at line
(defun find-file-at-line (file line)
  "Open given file at given line"
  (find-file file)
  (goto-line line)
  )

;; fast goto-line
(global-set-key (kbd "s-g") 'goto-line)

;; Use breadcrumb for easy bookmarking
(pacmans-cload
 'breadcrumb
 "breadcrumb"
 '(lambda ()
    (global-set-key [(super f2)]            'bc-set)
    (global-set-key [(f2)]                  'bc-previous)
    (global-set-key [(shift f2)]            'bc-next)
    (global-set-key [(super shift f2)]      'bc-list)
    )
 '(lambda () (el-get-install "breadcrumb"))
)

;; use lazy key for quick search
(pacmans-cload
 'lazy-search
 "lazy-search"
 '(lambda () (global-set-key (kbd "M-y") 'lazy-search-menu))
 '(lambda () (auto-install-batch "lazy-search"))
)


;; Non case-sensitive searches
(setq case-fold-search t)

;; easily align code
(global-set-key (kbd "<f11>") 'align)
(global-set-key (kbd "s-<f11>") 'align-regexp)
(global-set-key (kbd "S-<f11>") 'align-entire)

;; To have the cursor always centered
;; (require 'centered-cursor-mode)
;; (global-centered-cursor-mode t)

;; to be able to use column editing :
;; (cua-mode t)
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (transient-mark-mode 1) ;; No region when it is not highlighted
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; fast file navigation through zap to char actions
;; (require 'fastnav)
;; (global-set-key (kbd "s-z") 'zap-up-to-char-forward)
;; (global-set-key (kbd "s-Z") 'zap-up-to-char-backward)
;; (global-set-key (kbd "s-j") 'jump-to-char-forward)
;; (global-set-key (kbd "s-J") 'jump-to-char-backward)
;; (global-set-key "\M-s" 'jump-to-char-forward)
;; (global-set-key "\M-S" 'jump-to-char-backward)
;; (global-set-key "\M-r" 'replace-char-forward)
;; (global-set-key "\M-R" 'replace-char-backward)
;; (global-set-key "\M-i" 'insert-at-char-forward)
;; (global-set-key "\M-I" 'insert-at-char-backward)
;; (global-set-key "\M-j" 'execute-at-char-forward)
;; (global-set-key "\M-J" 'execute-at-char-backward)
;; (global-set-key "\M-k" 'delete-char-forward)
;; (global-set-key "\M-K" 'delete-char-backward)
;; (global-set-key "\M-m" 'mark-to-char-forward)
;; (global-set-key "\M-M" 'mark-to-char-backward)


(provide 'edit-conf)