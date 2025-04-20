;;; -*- lexical-binding: t -*-
;;
;; emacs --batch --script .emacs.d/provision.el

(setq shields/add-lisp-dir-to-load-path nil)
(load-file ".emacs.d/early-init.el")
(add-to-list 'load-path ".emacs.d/lisp")
(load-file ".emacs.d/init.el")

(straight-pull-all)
(straight-check-all)
(straight-remove-unused-repos t)

(nerd-icons-install-fonts 1)
