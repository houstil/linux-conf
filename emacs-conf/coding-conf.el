;; always follow symlinks if the file is vc
(setq vc-follow-symlinks t)

;; set Sagem-Code tab length
(add-to-list 'load-path "~/.emacs.d/dtrt-indent")
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq tab-width 3)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 3)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 3)
            )
          )


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
;; (defun my-doxymacs-font-lock-hook ()
;;   (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;       (doxymacs-font-lock)))
;; (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; auto-pairs : to aumatically insert matching pairs
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers


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
  (set-indent-newline-and-indent)
  (highlight-parentheses-mode t)
  )

;; c-mode configuration
(add-hook 'c-mode-hook
  '(lambda ()
    ;; (auto-fill-mode 80)
    (hs-minor-mode t)
    (global-set-key (kbd "<f8>") 'hs-toggle-hiding)
    (c-subword-mode 1)
    (easy-coding-configuration)))

;; lisp-mode configuration
(defadvice eval-region (before slick-copy activate compile) "When called
  interactively with no active region, eval the whole buffer."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Region Evaluated") (list (point-min) (point-max)) (message "Buffer Evaluated"))))


(add-hook 'lisp-mode-hook (lambda ()
            (define-key lisp-mode-map [f5] 'eval-region)
            (easy-coding-configuration)))
(add-hook 'emacs-lisp-mode-hook (lambda ()
            (define-key emacs-lisp-mode-map [f5] 'eval-region)
            (easy-coding-configuration)))
(add-hook 'lisp-interaction-mode-hook (lambda ()
            (define-key lisp-interaction-mode-map [f5] 'eval-region)
            (easy-coding-configuration)))


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

;; (setq yas/prompt-functions '(yas/dropdown-prompt
;; 			     yas/ido-prompt
;; 			     yas/completing-prompt))
(setq yas/prompt-functions '(yas/ido-prompt
			     yas/completing-prompt))

;; Develop in ~/emacs.d/mysnippets, but also
;; try out snippets in ~/.emacs.d/snippets
(setq yas/root-directory '("~/.emacs.d/mysnippets"
                           "~/.emacs.d/snippets"))

;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)

;; build a quote from selected text :
(defun quote-region ()
  "Build a quote inserting file name and line number"
  (interactive)
  (if (buffer-file-name)
      (kill-new (concat buffer-file-name ":" (number-to-string (line-number-at-pos)) ":\n" (buffer-substring-no-properties (region-beginning) (region-end))))
    (message "This buffer has no name ")))
(global-set-key (kbd "s-q") 'quote-region)

(provide 'coding-conf)
