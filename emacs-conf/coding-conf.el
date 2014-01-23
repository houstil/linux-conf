;; set Sagem-Code tab length
(pacmans-cload 'dtrt-indent
               '(lambda () (dtrt-indent-mode 1))
               )

(setq tab-width 3)
(setq-default indent-tabs-mode nil)

(defun set-indent-newline-and-indent ()
  "Indent"
  (interactive)
  (local-set-key (kbd "RET") '(lambda () (interactive) (indent-according-to-mode) (newline-and-indent)))
  )

(defun set-indent-yank ()
  "Indent"
  (interactive)
  (local-set-key (kbd "M-v") '(lambda () (interactive) (yank) (indent-according-to-mode)))
  )

(defun easy-coding-configuration ()
  "A generic coding configuration for indentation, paren show ..."
  (interactive)
  (set-indent-yank)
  (show-paren-mode 1)
  (set-indent-newline-and-indent)
  )

;;;;;;;;;;;;;;;;;;;;;
;; C configuration ;;
;;;;;;;;;;;;;;;;;;;;;

(setq-default c-basic-offset 3)

;; auto-pairs : to aumatically insert matching pairs
(pacmans-cload 'autopair
               '(lambda ()
                  (autopair-global-mode) ;; to enable in all buffers
                  ;; to make sure that autopair indent properly on newline
                  (defadvice autopair-newline (after ap-indent-according-to-mode activate)
                    "Indent accordint to the current mode after a newline"
                    (interactive)(indent-according-to-mode))
                  ))


;; c-mode configuration
(add-hook 'c-mode-hook
          '(lambda ()
             (c-set-style "bsd")
             (setq c-basic-offset 3)
             (auto-fill-mode 80)
             (hs-minor-mode t)
             (global-set-key (kbd "<f8>") 'hs-toggle-hiding)
             (c-subword-mode 1)
             (easy-coding-configuration)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice eval-region (before slick-copy activate compile) "When called interactively with no active region, eval the whole buffer."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
                                                                       "Region Evaluated") (list (point-min) (point-max)) (message "Buffer Evaluated"))))

(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'rainbow-delimiters)

(defun lisps-config ()
  (interactive)
  (rainbow-delimiters-mode 1)
  (easy-coding-configuration)
  (paredit-mode 1))

(add-hook 'cider-repl-mode (lisps-config))
(add-hook 'lisp-mode-hook              (lambda () (lisps-config)))
(add-hook 'clojure-mode-hook           (lambda () (lisps-config)))
(add-hook 'emacs-lisp-mode-hook        (lambda () (lisps-config)))
(add-hook 'lisp-interaction-mode-hook  (lambda () (lisps-config)))

;; to find the right mode for clojure-script
(add-to-list 'auto-mode-alist '("\\.cljs.hl" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs"    . clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoHotKey script configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ahk-syntax-directory "C:/Program Files/AutoHotkey/Extras/Editors/Syntax/")
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
(autoload 'ahk-mode "ahk-mode" "mode for editing AutoHotKey scripts")
;; those advice on bracket keys are not to my taste
(add-hook 'ahk-mode-hook #'(lambda ()
               (push ?{
                     (getf autopair-dont-pair :comment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VisualBasicScript configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.vbs$" . visual-basic-mode))
(autoload 'visual-basic-mode "visual-basic-mode" "mode for editing VisualBasicScripts")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook '(lambda () (auto-complete-mode 1)))
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to yasnippet snippets for faster editing
;; we assume that yasnippet is already installed
;; Develop in ~/emacs.d/mysnippets, but also
;; try out snippets in ~/.emacs.d/snippets


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-script configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for scripts, we have to choose the unix coding system
(add-hook 'sh-mode-hook '(lambda () (set-buffer-file-coding-system 'unix)))

(provide 'coding-conf)

