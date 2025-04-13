;;; -*- lexical-binding: t -*-

;; Straight bootstrap, pasted from its README.md.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; Configure use-package to use straight.el
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Work around https://github.com/joaotavora/eglot/discussions/1436
(straight-use-package 'project)
(require 'project)
(straight-use-package 'flymake)
(require 'flymake)

(setq use-package-always-ensure t)
