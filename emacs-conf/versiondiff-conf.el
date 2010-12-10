
;;;;;;;;;;;
;; ediff ;;
;;;;;;;;;;;

(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'ediff-load-hook
          '(lambda ()
            (set-face-foreground
             ediff-current-diff-face-B "blue")
            (set-face-background
             ediff-current-diff-face-B "red")
            (make-face-italic
             ediff-current-diff-face-B)))

;; use git with egg for ediff git diff
(add-to-list 'load-path "~/.emacs.d/egg")
(require 'egg)

;; use psvn for in emacs svn use
;; (require 'psvn)
(require 'vc-svn)

(provide 'versiondiff-conf)


