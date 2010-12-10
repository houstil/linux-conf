
;; to run offlineimap in emacs
(require 'offlineimap)
;; launch an offlineimap process in the background
(offlineimap)

;; notmuch emacs interface
(require 'notmuch)

(setq notmuch-search-oldest-first nil)

;; notmuch quick access
(global-set-key (kbd "<f9>") 'notmuch-folder)

;; use bbdb to store mail
(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
(require 'bbdb) 
(bbdb-initialize)
(setq 
    bbdb-offer-save 1                        ;; 1 means save-without-asking
    bbdb-use-pop-up t                        ;; allow popups for addresses
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
    bbdb-completion-type nil                 ;; complete on anything
    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially
    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA

    bbdb-elided-display t                    ;; single-line addresses
    ;; auto-create addresses from mail
    bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
    )
	
;; to get mail addr completion while sending mail
(require 'message-x)
(setq message-x-body-function '(lambda () (interactive) (hippie-expand)))
(defadvice smart-tab (around message-x-if-message activate compile)
  "When called check if in message mode. If so call message-x-complete-name instead."
  (if (string= major-mode "message-mode")
      (message-x-tab)
    ad-do-it
    ))

;; set my mail adress
(setq user-mail-address "jean-louis.arnault@sagemcom.com")

;; If you use the default mail user agent.
(setq send-mail-function 'smtpmail-send-it)

;; Notify Incoming Mail with mail-notify
;; (require 'mail-notify)

;; If you use Message or Gnus.
(setq message-send-mail-function 'smtpmail-send-it)

;; Send mail using SMTP via mail.example.org.
(setq smtpmail-smtp-server "vzy08031.vzy.sagem")

;; jabber.el : a xmpp client in emacs
(add-to-list 'load-path "~/.emacs.d/emacs-jabber-0.8.0")
(load "jabber-autoloads")
(setq jabber-account-list '(("g178452@etl-services2.rmm.sagem")))


(provide 'mail-conf)
