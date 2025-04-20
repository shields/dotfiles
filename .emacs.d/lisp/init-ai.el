;;; init-ai.el --- AI and LLM integration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for AI tools, LLMs, and code assistants

;;; Code:

;; LLM integration
(use-package gptel
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  (gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (auth-source-pick-first-password :host "api.anthropic.com"))))

(use-package chatgpt-shell
  :config
  (setopt chatgpt-shell-anthropic-key (auth-source-pick-first-password :host "api.anthropic.com"))
  (setopt chatgpt-shell-openai-key (auth-source-pick-first-password :host "api.openai.com")))

;; Aider
(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  (setopt aider-args nil)
  :bind ("C-c a" . aider-transient-menu))

;; yasnippet
(use-package yasnippet
  ;; :hook (prog-mode . yas-minor-mode-on)
  )

(provide 'init-ai)
;;; init-ai.el ends here
