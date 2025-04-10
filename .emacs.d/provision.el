;; emacs --batch --script .emacs.d/provision.el

(load-file ".emacs.d/early-init.el")
(load-file ".emacs.d/init.el")

(straight-pull-all)

(nerd-icons-install-fonts 1)
