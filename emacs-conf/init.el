;;;;;;;;;;;;;;;;;;;;;;;
;; Services - Basics ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; start the emacs server
;; (require 'server)
;; (setq server-host (system-name)
;;       server-use-tcp t)

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set s-w to server-edit (to exit a emacsclient session)
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
             (local-set-key (kbd "s-w") 'server-edit)))

;; set the load-path
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/plugins")

;; to allow seemless edition in terminal mode
(global-set-key (kbd "C-@")  'set-mark-command)


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
(global-set-key (kbd "M-4") 'comment-box)

;; disabling this boring print command
(global-unset-key (kbd "C-p"))

;; to comment a line without selecting it
(defadvice comment-or-uncomment-region (before slick-copy activate compile)
  "When called interactively with no active region, comment or uncomment a single line instead."
  (interactive (if mark-active
		   (list (region-beginning) (region-end)) (message  "Co|Unco line")
		   (list (line-beginning-position) (line-beginning-position  2)))))

;; set history length
(setq history-length 250)

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
(global-set-key (kbd "s-T")    'multi-term-restart)

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

;; to see buffer as tab
;; (require 'tabbar)
;; (tabbar-mode t)
;; (global-set-key (kbd "s-u") 'tabbar-backward-tab)
;; (global-set-key (kbd "s-o") 'tabbar-forward-tab)
;; (global-set-key (kbd "s-U") 'tabbar-backward-group)
;; (global-set-key (kbd "s-O") 'tabbar-forward-group)

;; to enable directionnal frame movement
;; (require 'framemove)

;; elscreen - screen like windows management in emacs
(add-to-list 'load-path "~/.emacs.d/elscreen-1.4.6")
(setq elscreen-prefix-key "@")
(load "elscreen" "ElScreen" t)

;; forbid dabbrev to change case
(setq dabbrev-case-replace nil)

;; add an intersting completion
;; (require 'pabbrev)
;; (pabbrev-mode t)

;; on the top completion mechanism
;; (add-to-list 'load-path "~/.emacs.d/company/")
;; (autoload 'company-mode "company" nil t)

;; require smart tab : expand or indent on tab hit
(require 'smart-tab)
(global-set-key (kbd "<tab>") 'smart-tab)

;; special cases for tab
(add-hook 'compilation-mode-hook
          '(lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
             (local-set-key (kbd "<tab>") 'compilation-next-error)
	     ))

(add-hook 'org-mode-hook
          '(lambda ()
	     (run-at-time "1 sec" nil '(lambda ()
					 (local-unset-key (kbd "<tab>"))
					 ))
	     )
	  )

;; (add-hook 'help-mode-hook
;;           '(lambda ()
;; 	     (ergoemacs-local-set-key (kbd "M-n") 'forward-button)
;; 	     (ergoemacs-local-set-key (kbd "M-p") 'backward-button)
;; 	     ))

(add-hook 'term-mode-hook
          '(lambda ()
	     (ergoemacs-local-unset-key (kbd "M-u"))
	     (ergoemacs-local-unset-key (kbd "M-o"))
	     (ergoemacs-local-unset-key (kbd "M-e"))
	     (ergoemacs-local-unset-key (kbd "M-r"))
	     (ergoemacs-local-unset-key (kbd "M-d"))
	     (ergoemacs-local-unset-key (kbd "M-f"))
	     (ergoemacs-local-unset-key (kbd "M-i"))
	     (ergoemacs-local-unset-key (kbd "M-k"))
	     (ergoemacs-local-unset-key (kbd "M-j"))
	     (ergoemacs-local-unset-key (kbd "M-l"))
	     (ergoemacs-local-unset-key (kbd "M-J"))
	     (ergoemacs-local-unset-key (kbd "M-L"))
	     (ergoemacs-local-unset-key (kbd "M-g"))
	     (ergoemacs-local-unset-key (kbd "M-v"))
	     (ergoemacs-local-unset-key (kbd "<delete>"))
	     (ergoemacs-local-unset-key (kbd "M-<delete>"))
	     (ergoemacs-local-unset-key (kbd "C-a"))
	     (ergoemacs-local-unset-key (kbd "<backspace>"))
	     (ergoemacs-local-unset-key (kbd "<esc>"))
	     (run-at-time "1 sec" nil '(lambda ()
					 (local-unset-key (kbd "<tab>"))
					 (local-unset-key (kbd "|"))
					 ))
	     ;; (define-key elscreen-map "a"    '(lambda () (interactive) (term-send-raw-string "@")) )
	     (multi-term-keystroke-setup)
	     )
	  )

;; to get quick terminals :
(require 'multi-term)

(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/zsh")
        (anon-term (get-buffer "*ansi-term*")))
    ;; (message (concat "is-term : " is-term "\nis-running : " is-running "\nanon-term : " anon-term))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(global-set-key (kbd "<f12>") 'visit-ansi-term)
(global-set-key (kbd "S-<f12>") 'ansi-term)


;; keys that need to be forwarded to the terminal :
(setq term-bind-key-alist '(
 ("C-c C-c"		.	term-interrupt-subjob)
 ("C-p"			.	previous-line)
 ("C-n"			.	next-line)
 ("C-m"			.	term-send-raw)
 ("M-o"			.	term-send-forward-word)
 ("M-u"			.	term-send-backward-word)
 ("M-i"			.	term-send-up)
 ("M-k"			.	term-send-down)
 ("M-j"			.	term-send-left)
 ("M-l"			.	term-send-right)
 ("M-r"			.	term-send-forward-kill-word)
 ("M-e"			.	term-send-backward-kill-word)
 ("M-d"			.	term-send-backspace)
 ("M-<backspace>"	.	term-send-backward-kill-word)
 ("<backspace>"		.	term-send-backspace)
 ("M-f"			.	term-send-del)
 ("<delete>"		.	term-send-del)
 ("M-<delete>"		.	term-send-forward-kill-word)
 ("M-L"			.	term-send-end)
 ("M-J"			.	term-send-home)
 ("M-g"			.	term-send-kill-line-right)
 ("<tab>"		.	term-send-raw)
 ("s-:"			.	term-send-reverse-search-history)
 ("s-;"                 .       term-send-search-history)
 ("M-,"			.	term-send-input)
 ("M-."			.	comint-dynamic-complete)
 ("M-v"			.	term-send-yank)
 ("<esc>"		.	term-send-esc)
 ("C-z"			.	term-send-Cz)
 ("<S-return>"		.	term-send-return)
 ))

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

;; tramp authentification
(require 'tramp)
(setq tramp-default-method "sudo")

;; to use icicles
;; (add-to-list 'load-path "~/.emacs.d/icicles")
;; (require 'icicles)
;; (icy-mode 1)

;; enables ido rather than icicles
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

; use ido for each completion
;; (defvar ido-enable-replace-completing-read t)

;; (defadvice find-tag (around original-completing-read-only activate)
;;   (let (ido-enable-replace-completing-read) ad-do-it))

;; Replace completing-read wherever possible, unless directed otherwise
;; (defadvice completing-read
;;   (around use-ido-when-possible activate)
;;   (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
;;           (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
;;       ad-do-it
;;     (let ((allcomp (all-completions "" collection predicate)))
;;       (if allcomp
;;           (setq ad-return-value
;;                 (ido-completing-read prompt
;;                                allcomp
;;                                nil require-match initial-input hist def))
;;         ad-do-it))))

;; use undo-tree
(require 'undo-tree)

;; use lazy key for quick search
(require 'lazy-search)
(global-set-key (kbd "M-y") 'lazy-search-menu)

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
;; use psvn for in emacs svn use
(require 'psvn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Less important configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define a few aliases to get quick access to basic emacs function
(defalias 'eb 'ediff-buffers)
(defalias 'erw 'ediff-regions-wordwise)
(defalias 'gf 'grep-find)
(defalias 'te 'ansi-term)
(defalias 'mg 'mingus)
(defalias 'mini '(lambda () (interactive)
		   ;; (serial-term "/dev/ttyS0" 38400)
		   (serial-term "/dev/ttyS0" 115200)
		   (rename-buffer "*/dev/ttyS0*")
		   ))
(defalias 'rwm '(lambda () (interactive)
		  (chmod (buffer-file-name) 438)
		  (revert-buffer nil t)
		  ))

;; to easily revert a buffer
(global-set-key (kbd "s-r") '(lambda () (interactive) (revert-buffer nil t)))


;; smex, to easily searchf an emacs command
;; (setq smex-save-file "~/.emacs.d/smex.save") ;; keep my ~/ clean
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "s-a") 'smex)

;; Use breadcrumb for easy bookmarking
(require 'breadcrumb)
(global-set-key [(super f2)]            'bc-set)
(global-set-key [(f2)]                  'bc-previous)
(global-set-key [(shift f2)]            'bc-next)
(global-set-key [(super shift f2)]      'bc-list)

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

;; to open recent files
(require 'recentf)
(recentf-mode 1)

;; to quickly explore files and buffers
(require 'lusty-explorer)


;;;;;;;;;;;;;;;
;; Apparence ;;
;;;;;;;;;;;;;;;

;; to see a mini version of the current buffer :
;; (require 'minimap)

;; describe colors under the cursor
;;(require 'eyedropper)

(set-terminal-coding-system 'latin-1)
(set-keyboard-coding-system 'latin-1)
(set-language-environment 'latin-1)

;; to easily toggle the way emacs display long lines
(global-set-key (kbd "S-<f8>") 'toggle-truncate-lines)

;; to show useless whitespaces
;; (require 'show-wspace) ;

;; to show colored-matching parren
(require 'highlight-parentheses)
(highlight-parentheses-mode t)

;; to show parentheses matching line in minibuffer
(require 'mic-paren) ; loading
(paren-activate)     ; activating

(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (paren-toggle-matching-quoted-paren 1)
                      (paren-toggle-matching-paired-delimiter 1)
                      (highlight-parentheses-mode t)
                      ;; (show-ws-highlight-trailing-whitespace)
)))

(add-hook 'c-mode-common-hook
          (function (lambda ()
                       (paren-toggle-open-paren-context 1)
                       (highlight-parentheses-mode t)
                       ;; (show-ws-highlight-trailing-whitespace)
                       )))

;; To show corresponding paren
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)

;; minimal fringe
(set-fringe-mode 3)

;; set default size
(set-face-attribute 'default nil :height 100)
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

;; hide if-def
;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (hide-ifdef-mode t)
;;            )
;; )

;; (setq hide-ifdef-initially t)

;; miscelanous variable


;; interpret and use ansi color codes in shell output windows
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ediff configuration
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-load-hook
          '(lambda ()
            (set-face-foreground
             ediff-current-diff-face-B "blue")
            (set-face-background
             ediff-current-diff-face-B "red")
            (make-face-italic
             ediff-current-diff-face-B)))

;; To have the cursor always centered
;; (require 'centered-cursor-mode)
;; (global-centered-cursor-mode t)

;; use mingus to pilot my mpd
(add-to-list 'load-path "~/.emacs.d/mingus")
(require 'mingus)

;; put the buffer file name on top of the kill ring
(defun kill-buffer-file-name ()
  "Copy the full file path that is associated with the buffer into the kill ring"
  (interactive)
  (if (buffer-file-name)
      (kill-new (buffer-file-name))
    (progn (message "This buffer has no file name !") (ding))
    ))

;; build a quote from selected text :
(defun quote-region ()
  "Build a quote inserting file name and line number"
  (interactive)
  (if (buffer-file-name)
      (kill-new (concat buffer-file-name ":" (number-to-string (line-number-at-pos)) ":\n" (buffer-substring-no-properties (region-beginning) (region-end))))
    (message "This buffer has no name ")))
(global-set-key (kbd "s-q") 'quote-region)



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; always follow symlinks if the file is vc
(setq vc-follow-symlinks t)

;; set Sagem-Code tab length
(add-to-list 'load-path "~/.emacs.d/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)
;; (setq tab-width 3)
;; (setq-default indent-tabs-mode nil)
;; (setq-default c-basic-offset 3)

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; (load-file "/usr/share/emacs23/site-lisp/cedet-1.0pre7/common/cedet.el")

;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;; (semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;; (semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)

;; Emacs Code Browser
;; (add-to-list 'load-path "/usr/share/emacs23/site-lisp/ecb-2.40/")
;; (require 'ecb)

;; to be able to debug through gud and gdb
(setq gud-chdir-before-run nil)
(setq gdb-use-separate-io-buffer t)

;; doxygen hook
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; org-mode settings
(global-font-lock-mode 1)                         ; for all buffers
(global-set-key "\C-ca" 'org-agenda)              ; to easily acces the agenda
(add-hook 'org-mode-hook 'turn-on-font-lock)      ; Org buffers only
;; (add-hook 'org-mode-hook 'auto-fill-mode)
					; Org buffers only
;; (add-hook 'org-mode-hook 'show-ws-highlight-trailing-whitespace)      ; Org buffers only

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-agenda-files (list "~/org/work.org"))

;; to hide leadings stars
(setq org-hide-leading-stars t)
;; Scheduled entries. Many users turn this on.
(setq org-agenda-todo-ignore-scheduled t)
;; don't show scheduled item in agenda
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-with-date t)
;; Deadlines. Many users turn this on.
(setq org-agenda-skip-deadline-if-done t)
;; ignore subentries of a TODO entry
(setq org-agenda-todo-list-sublevels nil)

;; ;; extend the todo items states
;; (setq org-todo-keywords
;;       '((sequence "TODO" "FEEDBACK"  "SCHEDULED""" "|" "UNDEFINED" """DONE" "DELEGATED")))

;; ;; and set colors for them
;; (setq org-todo-keyword-faces
;;       '(
;;         ("UNDEFINED" . (:foreground "gray" :weight bold))
;;         ("TODO" . (:foreground "orange red" :weight bold))
;;         ("FEEDBACK" . (:foreground "orange" :weight bold))
;;         ("SCHEDULED" . (:foreground "beige" :weight bold))
;;         ("DELEGATED" . (:foreground "yellow" :weight bold))
;;         ("DONE" . (:foreground "chartreuse" :weight bold))

;;         ))

;; auto-pairs : to aumatically insert matching pairs
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers


;; c-mode configuration
(add-hook 'c-mode-hook
  '(lambda ()
    ;; (auto-fill-mode 80)
    (hs-minor-mode t)
    (global-set-key (kbd "<f8>") 'hs-toggle-hiding)
    (c-subword-mode 1)
    )
  )

;; lisp-mode configuration
(defadvice eval-region (before slick-copy activate compile) "When called
  interactively with no active region, eval the whole buffer."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Region Evaluated") (list (point-min) (point-max)) (message "Buffer Evaluated"))))

(add-hook 'lisp-mode-hook
          '(lambda ()
            (define-key lisp-mode-map [f5] 'eval-region)
            (highlight-parentheses-mode t)
            ;; (show-ws-highlight-trailing-whitespace)
            )
          )

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
            (define-key emacs-lisp-mode-map [f5] 'eval-region)
            (highlight-parentheses-mode t)
            ;; (show-ws-highlight-trailing-whitespace)
            )
          )

(add-hook 'lisp-interaction-mode-hook
          '(lambda ()
            (define-key lisp-interaction-mode-map [f5] 'eval-region)
            (highlight-parentheses-mode t)
            ;; (show-ws-highlight-trailing-whitespace)
            )
          )

;; (defun sh-eval-region ()
;;   "eval a marked region as a shell command"
;;   (interactive)
;;   ()
;;   )

;; to easily test shell scripts or expressions
;; (add-hook 'sh-mode-hook
;;           '(lambda ()
;;             )
;;           )


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


;; to yasnippet snippets for faster editing
(setq yas/trigger-key "M-@")
(setq yas/wrap-around-region t)
(setq yas/fallback-behavior nil)
(require 'yasnippet-bundle)

;; Develop in ~/emacs.d/mysnippets, but also
;; try out snippets in ~/.emacs.d/snippets
(setq yas/root-directory '("~/.emacs.d/mysnippets"
                           "~/.emacs.d/snippets"))

;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)

;; to save your last place in a file at closing
(require 'saveplace)                          ;; get the package
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers


;; emacs desktop to save multiple emacs windows configuration
;; (setq desktop-dirname "~/.emacs.d/desktop")
;; (desktop-save-mode 1)

(defun hide-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;; align using space, not tabs
(setq align-default-spacing 1)

;; to be able to use column editing :
;; (cua-mode t)
;; (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;; (transient-mark-mode 1) ;; No region when it is not highlighted
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;;;;;;;;;;;;;;;;;;;;;;;;
;; Error-notification ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)
;; (color-theme-comidia)


