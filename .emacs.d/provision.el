(load "~/.emacs.d/package-repos.el")
(package-initialize)
(package-refresh-contents nil)

(package-install 'aggressive-indent)
(package-install 'all-the-icons)
(package-install 'company)
(package-install 'company-go)
(package-install 'doom-modeline)
(package-install 'exec-path-from-shell)
(package-install 'flx-ido)
(package-install 'flycheck)
(package-install 'go-eldoc)
(package-install 'go-mode)
(package-install 'magit)
(package-install 'markdown-mode)
(package-install 'multi-term)
(package-install 'projectile)
(package-install 'smartparens)
(package-install 'yasnippet)

(all-the-icons-install-fonts 1)
