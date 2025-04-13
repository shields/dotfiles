;;; -*- lexical-binding: t -*-
;;
;; emacs --batch --script .emacs.d/provision.el

(load-file ".emacs.d/early-init.el")
(load-file ".emacs.d/init.el")

(straight-pull-all)

(straight-remove-unused-repos t)

(nerd-icons-install-fonts 1)
