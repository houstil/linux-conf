;; define a few aliases to get quick access to basic emacs function
(defalias 'eb 'ediff-buffers)
(defalias 'erw 'ediff-regions-wordwise)
(defalias 'gf 'grep-find)
(defalias 'te 'ansi-term)
(defalias 'mg 'mingus)
(defalias 'mif '(lambda () (interactive)
		   (serial-term "/dev/ttyS0" 115200)
		   (rename-buffer "*/dev/ttyS0*")
		   ))
(defalias 'mis '(lambda () (interactive)
		   (serial-term "/dev/ttyS0" 38400)
		   (rename-buffer "*/dev/ttyS0*")
		   ))
(defalias 'rwm '(lambda () (interactive)
		  (chmod (buffer-file-name) 438)
		  (revert-buffer nil t)
		  ))

(provide 'aliases-conf)