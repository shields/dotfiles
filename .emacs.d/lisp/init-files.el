;;; init-files.el --- File and buffer handling -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Settings for file and buffer management

;;; Code:

;; Make sure editing a hard-linked file edits all its links.
(setopt backup-by-copying-when-linked t)

;; Only switch to existing buffers interactively
(defun shields/switch-to-buffer-existing-only (orig-fun &rest args)
  "When called interactively switch to existing buffers only, unless
when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg))))
  (apply orig-fun args))

(advice-add 'switch-to-buffer :around #'shields/switch-to-buffer-existing-only)

(setopt auto-save-default nil)
(setopt make-backup-files nil)
(setopt create-lockfiles nil)

(global-auto-revert-mode 1)
(setopt global-auto-revert-non-file-buffers t)
(setopt auto-revert-verbose nil)

;; This is only an issue when looking at the source for Emacs Lisp packages
;; installed via straight.
(setopt vc-follow-symlinks t)

(provide 'init-files)
;;; init-files.el ends here
