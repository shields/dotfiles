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
        eglot
        editorconfig
        eterm-256color
        exec-path-from-shell
        expand-region
        fancy-compilation
        flx
        flycheck
        go-mode
        goto-last-change
        gptel
        hl-todo
        jsonnet-mode
        magit
        magit-delta
        markdown-mode
        minions
        multi-term
        posframe
        ruff-format
        rustic
        smartparens
        straight
        swift-mode
        symbol-overlay
        terraform-mode
        tree-sitter
        typo
        use-package
        yaml-mode
        yasnippet))

(package-install-selected-packages t)

(straight-pull-all)

(nerd-icons-install-fonts 1)

