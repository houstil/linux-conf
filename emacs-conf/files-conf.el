;; to automatically byte-compile lisp-code
(pacmans-cload 'auto-compile
               '(lambda ()
                  (add-hook 'emacs-lisp-mode-hook '(lambda () (auto-compile-mode)))
                  ))

;; always follow symlinks if the file is vc
(setq vc-follow-symlinks t)

;; backup files in one folder
(setq backup-directory-alist `(("." . "~/.saves")))

;; use magit for git and svn
(require 'magit)

(provide 'files-conf)
