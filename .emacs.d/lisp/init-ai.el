;;; init-ai.el --- AI and LLM integration -*- lexical-binding: t -*-

;; Copyright © 2024-2026 Michael Shields
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
  (gptel-model 'claude-sonnet-5)
  (gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (auth-source-pick-first-password :host "api.anthropic.com"))))

(use-package chatgpt-shell
  :config
  ;; A key is a string or a function; setting nil, as when authinfo has no entry,
  ;; makes `setopt' warn on every start.
  (when-let* ((key (auth-source-pick-first-password :host "api.anthropic.com")))
    (setopt chatgpt-shell-anthropic-key key))
  (when-let* ((key (auth-source-pick-first-password :host "api.openai.com")))
    (setopt chatgpt-shell-openai-key key))

  ;; A hex string, not "white": a terminal frame maps the name to palette color
  ;; 7, which is #e5e5e5.
  (define-advice chatgpt-shell--fader-palette
      (:around (palette) shields/terminal-background)
    "Fade to white in a terminal, whose default face names no background."
    (let ((white (lambda (face-background face &rest args)
                   (let ((color (apply face-background face args)))
                     (if (and (eq face 'default) (equal color "unspecified-bg"))
                         "#ffffff"
                       color)))))
      (advice-add 'face-background :around white)
      (unwind-protect (funcall palette)
        (advice-remove 'face-background white)))))

(provide 'init-ai)
;;; init-ai.el ends here
