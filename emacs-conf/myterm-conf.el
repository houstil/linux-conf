
;; to get quick terminals :
(require 'multi-term)

;;;;;;;;;;;;;;;;;;;
;; configuration ;;
;;;;;;;;;;;;;;;;;;;

;; to get a autoscrolling terminal :
(setq term-scroll-show-maximum-output t)

;; interpret and use ansi color codes in shell output windows
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ergoemacs compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'term-mode-hook
          '(lambda ()
	     (ergoemacs-local-unset-key (kbd "M-u"))
	     (ergoemacs-local-unset-key (kbd "M-o"))
	     (ergoemacs-local-unset-key (kbd "M-e"))
	     (ergoemacs-local-unset-key (kbd "M-r"))
	     (ergoemacs-local-unset-key (kbd "M-d"))
	     (ergoemacs-local-unset-key (kbd "M-f"))
	     (ergoemacs-local-unset-key (kbd "M-i"))
	     (ergoemacs-local-unset-key (kbd "M-k"))
	     (ergoemacs-local-unset-key (kbd "M-j"))
	     (ergoemacs-local-unset-key (kbd "M-l"))
	     (ergoemacs-local-unset-key (kbd "M-J"))
	     (ergoemacs-local-unset-key (kbd "M-L"))
	     (ergoemacs-local-unset-key (kbd "M-g"))
	     (ergoemacs-local-unset-key (kbd "M-v"))
	     (ergoemacs-local-unset-key (kbd "<delete>"))
	     (ergoemacs-local-unset-key (kbd "M-<delete>"))
	     (ergoemacs-local-unset-key (kbd "C-a"))
	     (ergoemacs-local-unset-key (kbd "<backspace>"))
	     (ergoemacs-local-unset-key (kbd "C-<esc>"))
	     (run-at-time "1 sec" nil '(lambda ()
					 (local-unset-key (kbd "<tab>"))
					 (local-unset-key (kbd "|"))
					 ))
	     ;; (define-key elscreen-map "a"    '(lambda () (interactive) (term-send-raw-string "@")) )
	     (multi-term-keystroke-setup)
	     )
	  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom functions for multi-term ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-term-restart ()
  "Restart the current term buffer"
  (interactive)
  (let ((term-buffer-name (buffer-name) )
        (term-currrent-dir default-directory))
    (kill-buffer term-buffer-name)
    (setq default-directory term-currrent-dir)
    (ansi-term "/bin/zsh")
    (rename-buffer term-buffer-name)))

(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/zsh")
        (anon-term (get-buffer "*ansi-term*")))
    ;; (message (concat "is-term : " is-term "\nis-running : " is-running "\nanon-term : " anon-term))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special keys forwarding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq term-bind-key-alist '(
 ("C-c C-c"		.	term-interrupt-subjob)
 ("C-p"			.	previous-line)
 ("C-n"			.	next-line)
 ("C-m"			.	term-send-raw)
 ("M-o"			.	term-send-forward-word)
 ("M-u"			.	term-send-backward-word)
 ("M-i"			.	term-send-up)
 ("M-k"			.	term-send-down)
 ("M-j"			.	term-send-left)
 ("M-l"			.	term-send-right)
 ("M-r"			.	term-send-forward-kill-word)
 ("M-e"			.	term-send-backward-kill-word)
 ("M-d"			.	term-send-backspace)
 ("M-<backspace>"	.	term-send-backward-kill-word)
 ("<backspace>"		.	term-send-backspace)
 ("M-f"			.	term-send-del)
 ("<delete>"		.	term-send-del)
 ("M-<delete>"		.	term-send-forward-kill-word)
 ("M-L"			.	term-send-end)
 ("M-J"			.	term-send-home)
 ("M-g"			.	term-send-kill-line-right)
 ("<tab>"		.	term-send-raw)
 ("s-:"			.	term-send-reverse-search-history)
 ("s-;"                 .       term-send-search-history)
 ("M-,"			.	term-send-input)
 ("M-."			.	comint-dynamic-complete)
 ("M-v"			.	term-send-yank)
 ("C-<esc>"		.	term-send-esc)
 ("C-z"			.	term-send-Cz)
 ("<S-return>"		.	term-send-return)
 ))


(provide 'myterm-conf)