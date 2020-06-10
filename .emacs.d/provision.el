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
	company-tabnine
	counsel
	dockerfile-mode
	doom-modeline
	ethan-wspace
	exec-path-from-shell
	expand-region
	flx
	go-eldoc
	goto-last-change
	hl-todo
	ivy
	magit
	markdown-mode
	minions
	multi-term
	projectile
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
	 eterm-256color
	 flycheck
	 go-mode
	 lsp-mode
	 lsp-ivy
	 lsp-treemacs
	 lsp-ui
	 magit-delta
	 smartparens
	 treemacs)))

(package-install-selected-packages)

(all-the-icons-install-fonts 1)

(require 'company-tabnine)
(company-tabnine-install-binary)
