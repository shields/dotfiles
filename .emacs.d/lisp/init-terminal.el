;;; init-terminal.el --- Terminal configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

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
