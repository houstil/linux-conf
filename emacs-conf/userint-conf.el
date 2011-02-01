;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; to use icicles
;; (add-to-list 'load-path "~/.emacs.d/icicles")
;; (require 'icicles)
;; (icy-mode 1)

;; set history length
(setq history-length 250)

;; enables ido rather than icicles
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

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
;; (defvar ido-enable-replace-completing-read t)

;; smex, to easily searchf an emacs command
;; (setq smex-save-file "~/.emacs.d/smex.save") ;; keep my ~/ clean
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "s-a") 'smex)

(provide 'userint-conf)

