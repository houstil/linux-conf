;; light emacs configuration for root use

;;;;;;;;;;;;;;;;;;;;;;;
;; Services - Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; start the emacs server
;; (server-start)

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set the load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/plugins")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to easily distinct homonyme buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; load ErgoEmacs keybinding
(load "~/.emacs.d/ergoemacs-keybindings-5.1/ergoemacs-mode")

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
(global-set-key (kbd "<f6>") 'comment-or-uncomment-region)
(global-set-key (kbd "s-<f6>") 'comment-box)

;; to comment a line without selecting it
(defadvice comment-or-uncomment-region (before slick-copy activate compile) "When called
  interactively with no active region, comment or uncomment a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Co|Unco line") (list (line-beginning-position) (line-beginning-position
  2)))))

;; set history length
(setq history-length 250)

;; set c-tab to switch to the last used buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-<tab>")  'switch-to-previous-buffer)
(global-set-key (kbd "S-<f6>")   'ibuffer)
(global-set-key (kbd "<f6>")     'ido-switch-buffer)
(global-set-key (kbd "C-<f6>")   'lusty-buffer-explorer)
(global-set-key (kbd "M-S-<f6>") 'buffer-menu)
(global-set-key (kbd "s-<f6>")   'rename-buffer)

(global-set-key (kbd "M-n")      'ido-switch-buffer)
(global-set-key (kbd "s-n")      'visit-ansi-term)
(global-set-key (kbd "s-N")      'ansi-term)

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
  "find . -path '*/.svn' -prune -o -type f -print | xargs -e grep -i -n -e ")


;; to easily define macros
(global-set-key (kbd "M-<f3>") 'kmacro-start-macro)
(global-set-key (kbd "S-<f3>") 'kmacro-end-macro)
(global-set-key (kbd "<f3>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "s-<f3>") 'kmacro-insert-counter)
(global-set-key (kbd "s-S-<f3>") 'kmacro-set-counter)


;; some usefull toggeling
(global-set-key (kbd "<f4>") 'insert-register)
(global-set-key (kbd "<S-f4>") 'copy-to-register)

;;


;; set copy-paste from/to clipboard
;; (global-set-key (kbd "s-v") 'clipboard-yank)
;; (global-set-key (kbd "s-c") 'clipboard-kill-ring-save)

;; enable the use of the x clipboard
(setq x-select-enable-clipboard t)

;; forbid dabbrev to change case
(setq dabbrev-case-replace nil)

;; require smart tab : expand or indent on tab hit
(require 'smart-tab)
(global-set-key (kbd "<tab>") 'smart-tab)

;; smart-tab use hippie expand :
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Non case-sensitive searches
(setq case-fold-search t)

;; fast goto-line
(global-set-key (kbd "s-g") 'goto-line)

;; enables ido rather than icicles
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; use lazy key for quick search
(require 'lazy-search)
(global-set-key (kbd "M-y") 'lazy-search-menu)

(require 'ffap-)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Less important configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define a few aliases to get quick access to basic emacs function
(defalias 'eb 'ediff-buffers)
(defalias 'gf 'grep-find)
(defalias 'rwm '(lambda () (interactive)
		  (chmod (buffer-file-name) 438)
		  (revert-buffer nil t)
		  ))

;; to easily revert a buffer
(global-set-key (kbd "s-r") '(lambda () (interactive) (revert-buffer nil t)))


;; to quickly jump to last change :
(require 'goto-chg)
(global-set-key (kbd "s-h") 'goto-last-change)
(global-set-key (kbd "s-H") 'goto-last-change-reverse)

;; Enhance bookmark :
(setq
  bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
  bookmark-save-flag 1)                        ;; autosave each change)

(require 'bookmark+)
(global-set-key (kbd "<f10>") 'bookmark-jump)
(global-set-key (kbd "C-<f10>") 'bookmark-set)
(global-set-key (kbd "s-<f10>") 'bookmark-bmenu-list)

;; easily align code
(global-set-key (kbd "<f11>") 'align)
(global-set-key (kbd "s-<f11>") 'align-regexp)
(global-set-key (kbd "S-<f11>") 'align-entire)

;;;;;;;;;;;;;;;
;; Apparence ;;
;;;;;;;;;;;;;;;

(set-terminal-coding-system 'latin-1)
(set-keyboard-coding-system 'latin-1)
(set-language-environment 'latin-1)

;; to easily toggle the way emacs display long lines
(global-set-key (kbd "S-<f8>") 'toggle-truncate-lines)

;; to show colored-matching parren
(require 'highlight-parentheses)
(highlight-parentheses-mode t)

;; to show parentheses matching line in minibuffer
(require 'mic-paren) ; loading
(paren-activate)     ; activating

;; To show corresponding paren
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)

;; minimal fringe
(set-fringe-mode 3)

;; set default size
(set-face-attribute 'default nil :height 90)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; put the buffer file name on top of the kill ring
(defun kill-buffer-file-name ()
  "Copy the full file path that is associated with the buffer into the kill ring"
  (interactive)
  (if (buffer-file-name)
      (kill-new (buffer-file-name))
    (progn (message "This buffer has no file name !") (ding))
    ))
(global-set-key (kbd "s-n") 'kill-buffer-file-name)


;; auto-pairs : to aumatically insert matching pairs
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;; to make scrips executable on save
;; Check for shebang magic in file after save, make executable if found.
(setq my-shebang-patterns
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
            "^#!/usr/.*/sh"
            "^#!/usr/.*/bash"
            "^#!/bin/sh"
            "^#!/bin/bash"))
(add-hook
 'after-save-hook
 (lambda ()
   (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
       (progn
         ;; This puts message in *Message* twice, but minibuffer
         ;; output looks better.
         (message (concat "Wrote " (buffer-file-name)))
         (save-excursion
           (goto-char (point-min))
           ;; Always checks every pattern even after
           ;; match.  Inefficient but easy.
           (dolist (my-shebang-pat my-shebang-patterns)
             (if (looking-at my-shebang-pat)
                 (if (= (shell-command
                         (concat "chmod u+x " (buffer-file-name)))
                        0)
                     (message (concat
                               "Wrote and made executable "
                               (buffer-file-name))))))))
     ;; This puts message in *Message* twice, but minibuffer output
     ;; looks better.
     (message (concat "Wrote " (buffer-file-name))))))

;; auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Error-notification ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-theme)
;; (require 'zenburn)
;; (color-theme-zenburn)
;; (color-theme-comidia)
(color-theme-clarity)
