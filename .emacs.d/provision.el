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
	diff-hl
	dockerfile-mode
	doom-modeline
	eterm-256color
	ethan-wspace
	exec-path-from-shell
	expand-region
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
	symbol-overlay
	terraform-mode
	yasnippet))
(package-install-selected-packages)

(all-the-icons-install-fonts 1)
