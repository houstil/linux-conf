;; define a few aliases to get quick access to basic emacs function

(defalias 'eb 'ediff-buffers)

(defalias 'erw 'ediff-regions-wordwise)

(defalias 'gf 'grep-find)

(defalias 'te 'ansi-term)

(defalias 'mif '(lambda () (interactive) 
		  (serial-term "COM1" 115200)
		  (term-line-mode)))


(provide 'aliases-conf)
