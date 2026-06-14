;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t -*-

;; Copyright © 2018, 2020, 2025 Michael Shields
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
;; Configuration for terminal emulation

;;; Code:

;; Terminal configuration
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package multi-term
  :custom
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-m" . term-send-return)
     ("s-v" . term-paste)
     ("C-r" . term-send-reverse-search-history)
     ("M-." . comint-dynamic-complete)))
  (term-unbind-key-list
   '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "C-r" "C-s" "C-t"))
  (term-suppress-hard-newline t))

;; Enable the mouse for terminals
(xterm-mouse-mode 1)

(provide 'init-terminal)
;;; init-terminal.el ends here
