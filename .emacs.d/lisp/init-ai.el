;;; init-ai.el --- AI and LLM integration -*- lexical-binding: t -*-

;; Copyright © 2024 Michael Shields
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
;; Configuration for AI tools, LLMs, and code assistants

;;; Code:

;; LLM integration
(use-package gptel
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  :custom
  (gptel-model 'claude-sonnet-4-6)
  (gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (auth-source-pick-first-password :host "api.anthropic.com"))))

(use-package chatgpt-shell
  :config
  (setopt chatgpt-shell-anthropic-key (auth-source-pick-first-password :host "api.anthropic.com"))
  (setopt chatgpt-shell-openai-key (auth-source-pick-first-password :host "api.openai.com")))

(provide 'init-ai)
;;; init-ai.el ends here
