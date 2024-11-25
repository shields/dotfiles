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
        chatgpt-shell
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
        jsonnet-mode
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
        straight
        symbol-overlay
        terraform-mode
        tree-sitter
        typo
        use-package
        yaml-mode
        yasnippet))

(package-install-selected-packages t)

(nerd-icons-install-fonts 1)

(unless (file-exists-p (codeium-get-config 'codeium-command-executable nil nil))
  (codeium-install nil t))
