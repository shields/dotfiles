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
	counsel
	diff-hl
	dockerfile-mode
	doom-modeline
	eterm-256color
	ethan-wspace
	exec-path-from-shell
	expand-region
	flx
	flycheck
	go-eldoc
	go-mode
	hl-todo
	ivy
	magit
	markdown-mode
	minions
	multi-term
	projectile
	smartparens
	symbol-overlay
	terraform-mode
	yasnippet))

;; Some packages are in melpa-stable but only with very old versions.
(setq package-pinned-packages
      '((diff-hl . "melpa")
	(flycheck . "melpa")
	(go-mode . "melpa")
	(smartparens . "melpa")))

(package-install-selected-packages)

(all-the-icons-install-fonts 1)
