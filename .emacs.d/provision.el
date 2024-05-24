;; emacs -q --script .emacs.d/provision.el

(load "~/.emacs.d/package-repos.el")
(package-initialize)
(package-refresh-contents nil)

;; TODO: use use-package instead
(setq package-selected-packages
      '(aggressive-indent
	anzu
	apheleia
	avy
	company
	company-terraform
	counsel
	crux
	dap-mode
	diff-hl
	dockerfile-mode
	doom-modeline
	eterm-256color
	ethan-wspace
	expand-region
	flx
	flycheck
	flymake-ruff
	go-mode
	goto-last-change
	gptel
	hl-todo
	ivy
	jsonnet-mode
	lsp-ivy
	lsp-mode
	lsp-ui
	magit
	magit-delta
	markdown-mode
	minions
	multi-term
	posframe
	projectile
	ruff-format
	rustic
	smartparens
	symbol-overlay
	terraform-mode
	use-package
	yaml-mode
	yasnippet))

(package-install-selected-packages t)

(nerd-icons-install-fonts 1)

(load-file "codeium.el")
(codeium-install nil t)
