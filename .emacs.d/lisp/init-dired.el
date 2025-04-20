;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for directory editor and file management

;;; Code:

;; Dired configuration
(setopt dired-use-ls-dired t)

;; Use GNU ls from Homebrew, not BSD ls.
(when (file-executable-p "/opt/homebrew/bin/gls")
  (setopt insert-directory-program "/opt/homebrew/bin/gls"))

;; jka-compr for compressed files
(use-package jka-compr
  :config
  (jka-compr-install))

(provide 'init-dired)
;;; init-dired.el ends here
