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
	dockerfile-mode
	doom-modeline
	eterm-256color
	ethan-wspace
	exec-path-from-shell
	expand-region
	flx
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
	use-package
	yaml-mode
	yasnippet))

;; Some packages are in melpa-stable but we want a fresher version.
(setq package-pinned-packages
      (mapcar
       (lambda (package)
	 (push package package-selected-packages)
	 (cons package "melpa"))
       '(company-lsp
	 dap-mode
	 diff-hl
	 flycheck
	 go-mode
	 lsp-mode
	 lsp-ivy
	 lsp-treemacs
	 lsp-ui
	 smartparens
	 treemacs)))

(package-install-selected-packages)

(all-the-icons-install-fonts 1)
