;; Helper module to enhance el-get and elpa integration into my init scripts

;; let's add some proxy settings
(setq url-proxy-services '(("http"  . "localhost:3128") 
                          ("https" . "localhost:3128")))

(defun pacmans-recursive-add-subdirs (dir)
  "This add every subdirs of the given directory to the load path"
  (add-to-list 'load-path dir)
  (let ((default-directory  dir))
      (normal-top-level-add-subdirs-to-load-path)))

(defun pacmans-require-install (feature)
  "When a feature is not found, this should be used to install
  the providing module"
  (message (concat "Please install " feature))
)

(defun pacmans-cload (feature name postload install)
  (if (featurep feature)
      nil
    "Try to smartly load a feature."
    (condition-case nil
	(require feature)
      (error nil))
    (if (featurep feature)
      (when postload (funcall postload))
      (when install (progn (funcall install)
		 ;; let's try to load the newly installed package
		 (pacmans-cload feature name nil nil))
	)
    )
))


(pacmans-recursive-add-subdirs "~/.emacs.d/el-get")
(pacmans-recursive-add-subdirs "~/.emacs.d/elpa")
(pacmans-recursive-add-subdirs "~/.emacs.d/auto-install")

(pacmans-cload 'el-get "el-get" nil nil)
(pacmans-cload 'package "elpa" nil nil)
(pacmans-cload 'auto-install "auto-install" nil
               '(lambda () (el-get-install "auto-install")))


(provide 'pacmans-conf)
