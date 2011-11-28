;; to control where emacs put backup files :
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)


;; Enhance bookmark :
(setq
  bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
  bookmark-save-flag 1)                        ;; autosave each change)

;; (pacmans-cload
;;  'bookmark+
;;  "bookmark+"
;;  (lambda (progn 
;; 	   (global-set-key (kbd "<f10>") 'bookmark-jump)
;; 	   (global-set-key (kbd "C-<f10>") 'bookmark-set)
;; 	   (global-set-key (kbd "s-<f10>") 'bookmark-bmenu-list)
;; 	   )))

;; ;; set f12 to dired
;; (pacmans-cload 'dired+ "dired+"
;; 	       '(lambda () (toggle-diredp-find-file-reuse-dir 1)))

;; (global-set-key (kbd "s-<f12>") 'dired)



;; to easily distinct homonyme buffers
(pacmans-cload 'uniquify "uniquify"
 '(lambda () (setq uniquify-buffer-name-style 'post-forward))
 nil
 )


;; to open recent files
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(pacmans-cload 'recentf "recentf"
 '(lambda () (recentf-mode 1))
 nil
)

;; to quickly explore files and buffers
(pacmans-cload 'lusty-explorer "lusty-explorer"
 nil
 '(lambda () (package-install "lusty-explorer"))
)

;; to easily revert a buffer
(global-set-key (kbd "s-r") '(lambda () (interactive) (revert-buffer nil t)))

;; (pacmans-cload 'ffap- "ffap-" nil)

;; use mouse to open files
(global-set-key (kbd "s-<mouse-1>") 'ffap-other-window)
(global-set-key (kbd "s-o")         'ffap-other-window)

;; add advice to read the file number when using ffap-other-window
;; TODO integrate this functionnality in emacs-*/lisp/ffap.el
(defadvice ffap-other-window
; before see if we can find a file number after the point in
  (around search-line-number activate) 
	  "Parse the line at point looking for \":line_number:\" and use to go at the appropriate line number in the opened file"
  (let ((my-line-number (string-to-number (replace-regexp-in-string "\\(?1:.*:\\(?2:[[:digit:]]+\\):.*\\)" "\\2"
				(buffer-substring-no-properties (line-beginning-position) (line-beginning-position 2))
				nil nil 1)) ))
    ad-do-it
    (if my-line-number
	(goto-line my-line-number))
    )
)

;; put the buffer file name on top of the kill ring
(defun kill-buffer-file-name ()
  "Copy the full file path that is associated with the buffer into the kill ring"
  (interactive)
  (if (buffer-file-name)
      (kill-new (buffer-file-name))
    (progn (message "This buffer has no file name !") (ding))
    ))

;; to save your last place in a file at closing
(pacmans-cload 'saveplace "saveplace"
	       '(lambda () 
		  (setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
		  (setq-default save-place t)                   ;; activate it for all buffers
		  )
               ;; this feature should already exists in emacs 
	       nil)

(pacmans-cload 'sudo-save "sudo-save"
               nil
               '(lambda (el-get-install "sudo-save")))


;; THIS IS MY CONF
(provide 'files-conf)
