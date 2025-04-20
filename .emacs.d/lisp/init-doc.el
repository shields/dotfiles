;;; init-doc.el --- Documentation tools -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for documentation and help systems

;;; Code:

;; Eldoc for documentation
(setopt eldoc-idle-delay 0.1)

;; Enable multiple documentation sources.
(setopt eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;; Display in a "side" window at the bottom.
(setopt eldoc-display-functions '(eldoc-display-in-buffer))
(add-to-list 'display-buffer-alist
             '("^\\*eldoc"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.25)
               (window-parameters . ((no-delete-other-windows . t)))))

(global-eldoc-mode 1)

;; Info mode configuration
(use-package info
  :hook (Info-mode . variable-pitch-mode))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

(provide 'init-doc)
;;; init-doc.el ends here
