;;; init-prog-modes.el --- Programming language modes -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for various programming language modes

;;; Code:

;; C, C++, and Objective-C
(use-package c-ts-mode
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure))

(use-package cc-mode
  :hook
  (objc-mode . eglot-ensure))

;; Dockerfile
(use-package dockerfile-ts-mode)

;; Emacs Lisp
(use-package paren-face
  :hook emacs-lisp-mode

  :custom-face
  (parenthesis ((t (:inherit font-lock-bracket-face)))))

(define-key emacs-lisp-mode-map [(meta return)] #'eval-last-sexp)

(defun shields/eval-expression-minibuffer-setup ()
  (insert "()")
  (backward-char)
  ;; Don't smartparen-pair on '.
  (sp-update-local-pairs '(:open "'" :close nil :actions nil)))

(add-hook 'eval-expression-minibuffer-setup-hook
          #'shields/eval-expression-minibuffer-setup)

(defun shields/elisp-eldoc-with-value ()
  (remove-hook 'eldoc-documentation-functions
               #'elisp-eldoc-var-docstring t)
  (add-hook 'eldoc-documentation-functions
            #'elisp-eldoc-var-docstring-with-value nil t))
(add-hook 'emacs-lisp-mode-hook #'shields/elisp-eldoc-with-value)

;; If we have a copy of the Emacs source code, `describe-function' can browse
;; into it.
(let ((dir "~/src/emacs/src"))
  (when (file-readable-p (expand-file-name "emacs.c" dir))
    (setq find-function-C-source-directory dir)))

;; Go
(use-package go-ts-mode
  :custom
  (godoc-at-point-function #'godoc-gogetdoc)
  :hook
  (go-ts-mode . eglot-ensure)
  (go-ts-mode . shields/suppress-whitespace-mode)
  (go-mod-ts-mode . shields/suppress-whitespace-mode)
  :config
  (setf (alist-get 'go-dot-mod-mode major-mode-remap-alist) #'go-mod-ts-mode))

;; JavaScript and TypeScript
(use-package js
  :hook
  (js-ts-mode-hook . eglot-ensure))

(use-package typescript-ts-mode
  :hook
  (typescript-ts-mode-hook . eglot-ensure))

;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" #'compile)))

;; Perl
;; Make sure perl-mode doesn't even get loaded.
(defalias 'perl-mode 'cperl-mode)

(autoload 'describe-perl-symbol "perl-descr"
  "One-line information on a perl symbol" t)
(autoload 'switch-to-perl-doc-buffer "perl-descr"
  "One-line information on a perl symbol" t)

(eval-after-load "perl-mode"
  '(add-hook 'perl-mode-hook
             (function (lambda ()
                         (define-key perl-mode-map "\M-oq"
                                     'describe-perl-symbol)
                         (define-key perl-mode-map "\M-od"
                                     'switch-to-perl-doc-buffer)))))

(eval-after-load "cperl-mode"
  '(cperl-set-style "PerlStyle"))

;; Python
(use-package ruff-format)

;; Python configuration
;; "python" on macOS 10.15 is 2.7.
(setopt python-shell-interpreter "python3")

;; Python tree-sitter mode
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; Rust
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :after rust-mode
  :bind (:map rustic-mode-map
              ("M-?" . xref-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c a" . eglot-code-actions))
  :custom
  (rustic-analyzer-command '("rust-analyzer"))
  (rustic-lsp-client 'eglot)
  :hook
  (rustic-mode . eglot-ensure))

;; Shell Scripts
(use-package sh-script
  :hook
  (bash-ts-mode . eglot-ensure))

(setf (alist-get 'sh-mode major-mode-remap-alist) #'bash-ts-mode)

;; Swift
(use-package swift-mode
  :hook (swift-mode . eglot-ensure))

;; Terraform
(use-package terraform-mode)
;; Note: terraform-mode is excluded from aggressive-indent-mode in its configuration

(provide 'init-prog-modes)
;;; init-prog-modes.el ends here
