;; to be able to use tab for fold/unfol categories
(add-hook 'org-mode-hook
          '(lambda ()
	     (run-at-time "1 sec" nil '(lambda ()
					 (local-unset-key (kbd "<tab>"))
					 ))
	     )
	  )

;; org-mode settings
(global-font-lock-mode 1)                         ; for all buffers
(global-set-key "\C-ca" 'org-agenda)              ; to easily acces the agenda
(add-hook 'org-mode-hook 'turn-on-font-lock)      ; Org buffers only
;; (add-hook 'org-mode-hook 'auto-fill-mode)
					; Org buffers only

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(setq org-agenda-files (list "~/org/work.org"))

;; to hide leadings stars
(setq org-hide-leading-stars t)
;; Scheduled entries. Many users turn this on.
(setq org-agenda-todo-ignore-scheduled t)
;; don't show scheduled item in agenda
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-with-date t)
;; Deadlines. Many users turn this on.
(setq org-agenda-skip-deadline-if-done t)
;; ignore subentries of a TODO entry
(setq org-agenda-todo-list-sublevels nil)

;; ;; extend the todo items states
;; (setq org-todo-keywords
;;       '((sequence "TODO" "FEEDBACK"  "SCHEDULED""" "|" "UNDEFINED" """DONE" "DELEGATED")))

;; ;; and set colors for them
;; (setq org-todo-keyword-faces
;;       '(
;;         ("UNDEFINED" . (:foreground "gray" :weight bold))
;;         ("TODO" . (:foreground "orange red" :weight bold))
;;         ("FEEDBACK" . (:foreground "orange" :weight bold))
;;         ("SCHEDULED" . (:foreground "beige" :weight bold))
;;         ("DELEGATED" . (:foreground "yellow" :weight bold))
;;         ("DONE" . (:foreground "chartreuse" :weight bold))

;;         ))

(provide 'orgmode-conf)