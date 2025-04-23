;;; init-core.el --- Core Emacs configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Core initialization and preliminary settings

;;; Code:

;; Preliminaries
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Write customizations to a separate file instead of appending here.
(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; Disabled commands.  Hmmph.
(put 'eval-expression 'disabled nil)    ; M-ESC
(put 'narrow-to-region 'disabled nil)   ; C-x n n
(put 'set-goal-column 'disabled nil)    ; C-x C-n

;; Stop saying "You can run the command blah-blah with M-x bl-b".
(setopt extended-command-suggest-shorter nil)

(provide 'init-core)
;;; init-core.el ends here
