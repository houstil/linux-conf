;;;;;;;;;;;;;;;;;;;;;;;
;; Packages.el Items ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; this is the list of packages to check at install
;; to update, copy the value of 'package-activated-list'
(setq package-required-list '(ag auctex auto-compile auto-complete auto-install autopair color-theme-solarized color-theme dtrt-indent ergoemacs-mode goto-chg highlight-parentheses multi-term multiple-cursors org-cua-dwim packed magit git-rebase-mode git-commit-mode popup powerline psvn rainbow-delimiters smart-tab smex undo-tree yasnippet yasnippet-bundle zenburn-theme))

;; if some packages are missing, we try to install them automatically at startup
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package % is missing. Install it ?" package))
           (package-install package))))
 package-required-list)

(provide 'package-list)
