;; /usr/local/bin/emacs -q --script .emacs.d/provision.el

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
	company-lsp
	company-tabnine
	counsel
	dap-mode
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
	goto-last-change
	hl-todo
	ivy
	jsonnet-mode
	lsp-ivy
	lsp-mode
	lsp-treemacs
	lsp-ui
	magit
	magit-delta
	markdown-mode
	minions
	multi-term
	projectile
	smartparens
	symbol-overlay
	terraform-mode
	treemacs
	use-package
	yaml-mode
	yasnippet))

(package-install-selected-packages)

(all-the-icons-install-fonts 1)

(require 'company-tabnine)
(company-tabnine-install-binary)
