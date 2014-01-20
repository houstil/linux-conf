;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; disable this annoying bell
(setq visible-bell t)

;; to use icicles
;; (add-to-list 'load-path "~/.emacs.d/icicles")
;; (require 'icicles)
;; (icy-mode 1)

;; set history length
(setq history-length 250)

;; use auto-complete for completion
(pacmans-cload 'auto-complete
	       '(lambda () 
		  (auto-complete-mode 1)
		  (require 'auto-complete-config)
		  (ac-config-default))
               )

(require 'yasnippet)
(yas/global-mode 1)
(setq yas/root-directory "~/.emacs.d/snippets")
(setq yas/trigger-key "TAB")
(setq yas/trigger-key "<tab>")

(yas/load-directory yas/root-directory)

;; to avoid getting non working completion in my snippets
(add-hook 'yas/before-expand-snippet-hook '(lambda () (when (derived-mode-p 'lisp-mode)
                                                        (auto-complete-mode 0))))
(add-hook 'yas/after-exit-snippet-hook    '(lambda () (when (derived-mode-p 'lisp-mode)
                                                        (auto-complete-mode 1))))

;; enables ido rather than icicles
(pacmans-cload 'ido
               '(lambda ()
                  (ido-mode t)
                  (setq ido-enable-flex-matching t) ;; enable fuzzy matching

                  ;; disable new buffer while navigating in dired
                  (put 'dired-find-alternate-file 'disabled nil)  

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

                  ;; use ido for each completion
                  (defvar ido-enable-replace-completing-read t)

                  ;; use ergoemacs keys when in ido minibuffer
                  (add-hook 'ido-minibuffer-setup-hook
                            '(lambda ()
                               	(when (boundp 'ergoemacs-mode)
				 (when ergoemacs-mode
                               	 (progn
                                   (local-set-key (kbd "M-j") 'ido-prev-match)
                                   (local-set-key (kbd "M-l") 'ido-next-match)
                                   ))
                               	)))
		  )
	       )

                              
;; smex, to easily searchf an emacs command
(autoload 'smex "smex")
(if ergoemacs-mode
    (ergoemacs-key "M-a" 'smex)
  (global-set-key (kbd "M-x") 'smex))

(setq smex-save-file "~/.emacs.d/smex.save")

;; I don't want to use the cua mode
(cua-mode 0)

;; require clojure
(require 'clojure-mode)

;; let's add some key cords for some usefull commands
(pacmans-cload 'key-chord
               '(lambda ()
                  (key-chord-mode 1)
                  ;; global commands
                  (key-chord-define-global "zb" 'comment-box)
                  (key-chord-define-global "zj" 'join-line)
                                    
                  ;; global key chords
                  (key-chord-define-global "aa" 'smex)
                  (key-chord-define-global "xx" 'find-file)
                  (key-chord-define-global "yy" 'goto-last-change)
                  (key-chord-define-global "hh" 'ido-switch-buffer)
                  (key-chord-define-global "zz" 'save-buffer)
                  (key-chord-define-global "zb" 'comment-box)
                  (key-chord-define-global "LL" (lambda () (interactive) (join-line t)))
                  (key-chord-define-global "ww" 'yas/expand)
                  ;;  maybe there is a way to make that work ...
                  ;; (key-chord-define-global "gg" 'keyboard-quit)

                  ;; lisp keychords
                  (key-chord-define lisp-mode-map       "vv" (lambda () (interactive) (end-of-line) (slime-eval-last-expression-in-repl nil)))
                  (key-chord-define emacs-lisp-mode-map "vv" (lambda () (interactive) (end-of-line) (eval-last-sexp nil)))
                  (key-chord-define clojure-mode-map "vv" (lambda () (interactive) (end-of-line) (cider-eval-last-sexp nil)))
                  (key-chord-define clojure-mode-map "VV" (lambda () (interactive) (end-of-line) (cider-eval-expression-at-point nil)))
                  ))


(provide 'userint-conf)

