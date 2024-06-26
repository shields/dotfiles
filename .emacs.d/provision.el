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
	cape
	company
	company-terraform
	counsel
	crux
	dap-mode
	dash-at-point
	diff-hl
	dockerfile-mode
	doom-modeline
	editorconfig
	eterm-256color
	exec-path-from-shell
	expand-region
	fancy-compilation
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
	tree-sitter
	typo
	use-package
	yaml-mode
	yasnippet))

(package-install-selected-packages t)

(nerd-icons-install-fonts 1)

(load-file "codeium.el")
(codeium-install nil t)
