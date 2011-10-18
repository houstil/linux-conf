;; Helper module to enhance el-get and elpa integration into my init scripts

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

(defun pacmans-cload (feature name postload)
  (if (featurep feature)
      nil
    "Try to smartly load a feature."
    (condition-case nil
	(require feature)
      (error nil)
      )
    (if (featurep feature)
      (when postload (funcall postload))
      (message (concat "Please install " name))
    )
))


(pacmans-recursive-add-subdirs "~/.emacs.d/el-get")
(pacmans-recursive-add-subdirs "~/.emacs.d/elpa")

(pacmans-cload 'el-get "el-get" nil)
(pacmans-cload 'package "elpa" nil)


(provide 'pacmans-conf)