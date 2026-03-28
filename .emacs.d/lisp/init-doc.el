;;; init-doc.el --- Documentation tools -*- lexical-binding: t -*-

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
;; Configuration for documentation and help systems

;;; Code:

;; Eldoc for documentation
(setopt eldoc-idle-delay 0.1)

;; Enable multiple documentation sources.
(setopt eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;; Display in a "side" window at the bottom.
(setopt eldoc-display-functions '(eldoc-display-in-buffer))
(add-to-list 'display-buffer-alist
             '("^\\*eldoc"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.25)
               (window-parameters . ((no-delete-other-windows . t)))))

(global-eldoc-mode 1)

;; Info mode configuration
(use-package info
  :hook (Info-mode . variable-pitch-mode))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

(provide 'init-doc)
;;; init-doc.el ends here
