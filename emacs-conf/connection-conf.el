;; let's add some proxy settings
(setq url-proxy-services '(("http"  . "localhost:3128") 
                          ("https" . "localhost:3128")))

;; to run offlineimap in emacs
;; (pacmans-cload 'offlineimap "offlineimap" 
;; 	       '(lambda () 
;; 		  ;; launch an offlineimap process in the background
;; 		  (offlineimap)))



;; notmuch emacs interface
;; (pacmans-cload 'notmuch "notmuch" 
;; 	       '(lambda () 
;; 		  (setq notmuch-search-oldest-first nil)
;; 		  ;; notmuch quick access
;; 		  (global-set-key (kbd "<f9>") 'notmuch-folder)
;; 		  ))


;; use bbdb to store mail
;; (pacmans-cload 'bbdb "bbdb" 
;; 	       '(lambda ()
;; 		 (setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
;; 		 (bbdb-initialize)
;; 		 (setq 
;; 		  bbdb-offer-save 1                        ;; 1 means save-without-asking
;; 		  bbdb-use-pop-up t                        ;; allow popups for addresses
;; 		  bbdb-electric-p t                        ;; be disposable with SPC
;; 		  bbdb-popup-target-lines  1               ;; very small
;; 		  bbdb-dwim-net-address-allow-redundancy t ;; always use full name
;; 		  bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
;; 		  bbdb-always-add-address t                ;; add new addresses to existing...
;; 		  ;; ...contacts automatically
;; 		  bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
;; 		  bbdb-completion-type nil                 ;; complete on anything
;; 		  bbdb-complete-name-allow-cycling t       ;; cycle through matches
;; 		  ;; this only works partially
;; 		  bbbd-message-caching-enabled t           ;; be fast
;; 		  bbdb-use-alternate-names t               ;; use AKA

;; 		  bbdb-elided-display t                    ;; single-line addresses
;; 		  ;; auto-create addresses from mail
;; 		  bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
;; 		  bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
;; 		  ;; NOTE: there can be only one entry per header (such as To, From)
;; 		  ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

;; 		  '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
;; 		  )))
	
;; to get mail addr completion while sending mail
;; (pacmans-cload 'message-x "message-x"
;; 	       '(lambda () 
;; 		  (setq message-x-body-function '(lambda () (interactive) (hippie-expand)))
;; 		  (defadvice smart-tab (around message-x-if-message activate compile)
;; 		    "When called check if in message mode. If so call message-x-complete-name instead."
;; 		    (if (string= major-mode "message-mode")
;; 			(message-x-tab)
;; 		      ad-do-it
;; 		      ))))

;; set my mail adress
(setq user-mail-address "jean-louis.arnault@technicolor.com")

;; If you use the default mail user agent.
(setq send-mail-function 'smtpmail-send-it)

;; Notify Incoming Mail with mail-notify
;; (require 'mail-notify)

;; If you use Message or Gnus.
(setq message-send-mail-function 'smtpmail-send-it)

;; Send mail using SMTP via mail.example.org.
;;(setq smtpmail-smtp-server "vzy08031.vzy.sagem")

;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials
;;       (expand-file-name "~/.authinfo")
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-debug-info t)
;; (require 'smtpmail)

;; jabber.el : a xmpp client in emacs
(pacmans-cload 'jabber "jabber" nil '(lambda () (el-get-install "emacs-jabber")))


;; tramp authentification
(pacmans-cload 'tramp "tramp"
	       '(lambda () 
		  (setq tramp-default-method "ssh")
		  )
	       nil ;;tramp should already be in emacs 
)

;; use mingus to pilot my mpd
;; (pacmans-cload 'mingus "mingus" nil '(lambda () (el-get-install "mingus")))
		 

(provide 'connection-conf)
