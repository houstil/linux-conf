;;;;;;;;;;;;;;;;
;; multi-term ;;
;;;;;;;;;;;;;;;;

;; multi-term is there, we can apply some custo
(defun my-multiterm-custo ()
  (progn
    ;; to ensure   that now we don't erase full words in   ash
    (defadvice term-send-backward-kill-word (around term-send-backward-delim activate)
      (term-send-raw-string "\e\b"))

    (defun multi-term-restart ()
      "Restart the current term buffer"
      (interactive)
      (let ((term-buffer-name (buffer-name) )
	    (term-currrent-dir default-directory))
	(kill-buffer term-buffer-name)
	(setq default-directory term-currrent-dir)
	(ansi-term shell-file-name)
	(rename-buffer term-buffer-name)))

    (defun rename-dynb ()
      "Rename a dynamic buffer"
      (interactive)
      (rename-buffer (completing-read "Rename dynamic buffer (to new name): " nil nil nil '("**" . 1) () ))
      )

    (defun visit-ansi-term ()
      "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
      (interactive)
      (let ((is-term (string= "term-mode" major-mode))
	    (is-running (term-check-proc (buffer-name)))
	    (term-cmd shell-file-name)
	    (anon-term (get-buffer "*ansi-term*")))
	;; (message (concat "is-term : " is-term "\nis-running : " is-running "\nanon-term : " anon-term))
	(if is-term
	    (if is-running
		(if (string= "*ansi-term*" (buffer-name))
		    (call-interactively 'rename-dynb)
		  (if anon-term
		      (switch-to-buffer "*ansi-term*")
		    (ansi-term term-cmd)))
	      (kill-buffer (buffer-name))
	      (ansi-term term-cmd))
	  (if anon-term
	      (if (term-check-proc "*ansi-term*")
		  (switch-to-buffer "*ansi-term*")
		(kill-buffer "*ansi-term*")
		(ansi-term term-cmd))
	    (ansi-term term-cmd)))))

    ;; add an advice to keep a term buffer in the pile when closing the current one
    (defadvice close-current-buffer
      (around term-close-current-buffer-keep-termbuff activate)
      "check if the next buffer is a term and switch to if after killing the current buffer if so"
      (when (term-check-proc (other-buffer))
	(setq term-nextbuff (other-buffer))
	ad-do-it
	(if term-nextbuff
	    (switch-to-buffer term-nextbuff))
	)
      ad-do-it
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special keys forwarding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defmacro term-set-key (keysin func &optional keysout)
      "Bind keysin to the func or keysout in term mode"
      `(ergoemacs-local-set-key (kbd ,keysin) ,(if func
                                                   func
                                                 (when keysout `(lambda () (interactive) (term-send-raw-string ,keysout))))))
    
    (defun term-set-keys ()
      (interactive)
      (progn
        (when ergoemacs-mode
        (term-set-key "C-c C-c"       'term-interrupt-subjob)
        (term-set-key "C-j"           'next-line)
        (term-set-key "C-m"           'term-send-raw)
        (term-set-key "M-y"           'term-send-forward-word)
        (term-set-key "M-l"           'term-send-backward-word)
        (term-set-key "M-u"           'term-send-up)
        (term-set-key "M-e"           'term-send-down)
        (term-set-key "M-n"           'term-send-left)
        (term-set-key "M-i"           'term-send-right)
        (term-set-key "M-p"           'term-send-forward-kill-word)
        (term-set-key "M-f"           'term-send-backward-kill-word)
        (term-set-key "M-s"           'term-send-backspace)
        (term-set-key "M-<backspace>" 'term-send-backward-kill-word)
        (term-set-key "<backspace>"   'term-send-backspace)
        (term-set-key "M-t"           'term-send-del)
        (term-set-key "<delete>"      'term-send-del)
        (term-set-key "M-<delete>"    'term-send-forward-kill-word)
        (term-set-key "M-I"           'term-send-end)
        (term-set-key "M-N"           'term-send-home)
        (term-set-key "C-R"           nil "\C-r")
        (term-set-key "C-r"           nil "\C-s")
        (term-set-key "M-d"           nil "\C-k")
        (term-set-key "M-,"           'term-send-input)
        (term-set-key "M-."           'comint-dynamic-complete)
        (term-set-key "M-v"           'term-paste)
        (term-set-key "<tab>"         nil "\t")
        (term-set-key "C-`"           nil "\C-[")
        (term-set-key "C-z"           nil "\C-z")
        )))
    
    ;; add an advice to run after calling ansi-term
    (defadvice ansi-term
      (after term-ansi-disable-autopair activate)
      "disable autopair in a ainsi-term buffer"
      (interactive)
      (autopair-mode 0)
      (term-set-keys))

    (key-chord-define-global "TT" 'visit-ansi-term)
    (key-chord-define term-mode-map "qq" 'erase-buffer)
))

;; TODO : we should really use multi-term and not those functions

;; we can't use ansi-term under windows, it wont work
(if window-system
    (progn
      (setq shell-switcher-mode t))
  (progn
    (pacmans-cload 'multi-term 
                   'my-multiterm-custo)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to get a autoscrolling terminal :
(setq term-scroll-show-maximum-output t)

;; for the serial tty buffer
(setq comint-buffer-maximum-size 16384)
(setq serial-speed-history '("115200"))

;; interpret and use ansi color codes in shell output windows
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; to turn off the shell command echo
(add-hook 'comint-mode-hook '(lambda () (setq comint-process-echoes t)))

;; allowing to use erase-buffer
(put 'erase-buffer 'disabled nil)

(provide 'myterm-conf)
