;;; init-completion.el --- Completion framework configuration -*- lexical-binding: t -*-

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
;; Settings for minibuffer completion and in-buffer completion

;;; Code:

(global-completion-preview-mode 1)

(use-package vertico
  :config
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t)

  :custom-face
  (vertico-current ((t (:background "thistle1" :weight bold))))
  (vertico-group-title ((t (:weight bold)))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :config
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 86400)) ; 24 hours in seconds

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package flx)

;; Programming mode configuration
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'shields/prog-capf
                              #'cape-file
                              #'cape-dabbrev))))

(defun shields/prog-capf ()
  (cape-wrap-super #'eglot-completion-at-point))

(provide 'init-completion)
;;; init-completion.el ends here
