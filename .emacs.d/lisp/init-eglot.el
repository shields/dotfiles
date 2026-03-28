;;; init-eglot.el --- LSP client configuration -*- lexical-binding: t -*-

;; Copyright © 2025 Michael Shields
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; Configuration for Eglot LSP client

;;; Code:

;; Eglot LSP client configuration
(use-package eglot
  :config
  (setopt eglot-autoshutdown t)
  (setopt eglot-confirm-server-edits nil)
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("C-c l h" . eglot-help-at-point)
              ("C-c l i" . eglot-inlay-hints-mode))

  :custom-face
  (eglot-highlight-symbol-face ((t nil)))
  (eglot-inlay-hint-face ((t (:inherit nil :background "gray97" :foreground "gray20" :height 0.707)))))

;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
(setq-default eglot-workspace-configuration
              '(:gopls ((staticcheck . t)
                        (vulncheck . "Imports")
                        (hints . ((assignVariableTypes . t)
                                  (compositeLiteralFields . t)
                                  (compositeLiteralTypes . t)
                                  (functionTypeParameters . t)
                                  (parameterNames . t)
                                  (rangeVariableTypes . t))))))

;; Language-specific LSP server configurations

;; Use a current clangd, not the old one that comes with Xcode
(with-eval-after-load 'eglot
  (setf (alist-get '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                   eglot-server-programs)
        '("/opt/homebrew/opt/llvm/bin/clangd")))

;; Python server configuration
(with-eval-after-load 'eglot
  (setf (alist-get 'python-mode eglot-server-programs)
        '("basedpyright-langserver" "--stdio")))

;; Enable Ruff as an additional source of warnings. This calls
;; flymake-ruff-load for every Eglot mode, but it is a no-op for
;; non-Python modes.
(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

(provide 'init-eglot)
;;; init-eglot.el ends here
