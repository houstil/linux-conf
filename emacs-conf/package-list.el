;;;;;;;;;;;;;;;;;;;;;;;
;; Packages.el Items ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; this is the list of packages to check at install
;; to update, copy the value of 'package-activated-list'
(setq package-required-list '(auctex auto-compile auto-complete auto-install autopair clojure-mode color-theme-solarized color-theme cyberpunk-theme dtrt-indent ergoemacs-mode goto-chg highlight-parentheses highlight-symbol key-chord multi-term multiple-cursors org-cua-dwim packed magit git-rebase-mode git-commit-mode paredit popup powerline psvn rainbow-delimiters shell-switcher smart-tab smex undo-tree yasnippet yasnippet-bundle zenburn-theme))

;; if some packages are missing, we try to install them automatically at startup
(mapc
 (lambda (package)
   ;; (or (package-installed-p package)
   ;;     (if (y-or-n-p (format "Package %s is missing. Install it ?" package))
   ;;         (package-install package))
   (or (package-installed-p package)
           (package-install package))
   )
 package-required-list)

(provide 'package-list)
