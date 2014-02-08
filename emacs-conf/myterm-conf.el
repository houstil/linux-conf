;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; term configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun term-mode-conf ()
  (require 'term)
  "a lot of configuration for term-mode and ansi-term"
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
  (defadvice term-char-mode (around disable-autopairs-around (arg))
    "Disable autopairs mode if paredit-mode is turned on"
    ad-do-it
    (if (null ad-return-value)
        (autopair-mode 1)
      (autopair-mode 0)
      ))

  ;; to get a autoscrolling terminal :
  (setq term-scroll-show-maximum-output t)

  ;; allowing to use erase-buffer
  (put 'erase-buffer 'disabled nil)
  (key-chord-define term-mode-map "qq" 'erase-buffer)
  
  ) ;; and of the term-mode-conf function


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-switcher configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq shell-switcher-mode t)

;; in windows we can't use ansi-term, so we fall back on eshell, which the default for shell-switcher and we dont have to load the term configuration
(when (eq system-type 'gnu/linux)
  (term-mode-conf)
  (setq shell-switcher-new-shell-function
        (lambda () (multi-term))))

(when (eq system-type 'windows-nt)
  (setq win-bin-path "C:/Program Files (x86)/Git/bin/")
  (setq win-shell-name "bash.exe")
  (setq shell-file-name (concat win-bin-path win-shell-name))
  (setq explicit-shell-file-name shell-file-name)
  (add-to-list 'exec-path win-bin-path)
  (setenv "PATH" (concat (getenv "PATH") (concat ";" win-bin-path)))
  (setq binary-process-input t) 
  (setq w32-quote-process-args ?\") 
  (setenv "SHELL" shell-file-name) 
  (setq explicit-sh-args '("-login" "-i"))
  (setq shell-switcher-new-shell-function (lambda () (shell)))
  )

;; we start a new shell buffer so that its ready
(shell-switcher-new-shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other term-related conf ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for the serial tty buffer
(setq comint-buffer-maximum-size 16384)
(setq serial-speed-history '("115200"))

;; interpret and use ansi color codes in shell output windows
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; to turn off the shell command echo
(add-hook 'comint-mode-hook '(lambda () (setq comint-process-echoes t)))

(provide 'myterm-conf)
