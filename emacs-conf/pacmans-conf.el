;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to add additionnals repository
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defun pacmans-recursive-add-subdirs (dir)
  "This add every subdirs of the given directory to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory  dir))
      (normal-top-level-add-subdirs-to-load-path)))

(defun pacmans-cload (feature postload)
    "Try to smartly load a package"
    (if (require feature nil 'noerror)
	(when postload (funcall postload))))

;; (pacmans-recursive-add-subdirs "~/.emacs.d/el-get")
;; (pacmans-recursive-add-subdirs "~/.emacs.d/elpa")
;; (pacmans-recursive-add-subdirs "~/.emacs.d/auto-install")
(pacmans-recursive-add-subdirs "~/.emacs.d/dl-el")

(provide 'pacmans-conf)
