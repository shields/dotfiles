;;; init-compile.el --- Compilation configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for compilation mode and build tools

;;; Code:

;; Compilation mode settings
(setopt compilation-message-face 'default)
(setopt compilation-always-kill t)
(setopt compilation-scroll-output 'first-error)

(use-package fancy-compilation)

(use-package compile
  :config
  (fancy-compilation-mode))

(provide 'init-compile)
;;; init-compile.el ends here
