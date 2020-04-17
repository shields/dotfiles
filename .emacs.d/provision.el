(load "~/.emacs.d/package-repos.el")
(package-initialize)
(package-refresh-contents nil)

(setq package-selected-packages
      '(aggressive-indent
	all-the-icons
	anzu
	avy
	company
	company-go
	dockerfile-mode
	doom-modeline
	eterm-256color
	exec-path-from-shell
	flx-ido
	flycheck
	go-eldoc
	go-mode
	hl-todo
	ido-vertical-mode
	magit
	markdown-mode
	minions
	multi-term
	projectile
	smartparens
	terraform-mode
	yasnippet))
(package-install-selected-packages)

(all-the-icons-install-fonts 1)
