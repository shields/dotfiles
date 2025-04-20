;;; init-modeline.el --- Mode line configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for the mode-line appearance and functionality

;;; Code:

(use-package doom-modeline
  :straight (:fork "shields" :branch "vcs-font-custom")

  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-minor-modes t)
  (doom-modeline-number-limit 999)
  (doom-modeline-vcs-max-length 32)
  (doom-modeline-total-line-number t)
  (doom-modeline-position-column-line-format '("%c  %l"))
  (doom-modeline-height 21)
  (doom-modeline-spc-face-overrides '(:inherit (fixed-pitch)))
  ;; Pending my https://github.com/seagle0128/doom-modeline/pull/776:
  (doom-modeline-vcs-state-faces-alist
   '((needs-update . doom-modeline-warning)
     (removed . doom-modeline-urgent)
     (conflict . doom-modeline-urgent)
     (unregistered . doom-modeline-urgent)
     (edited . (doom-modeline-buffer-modified doom-modeline-info))
     (added . (doom-modeline-buffer-modified doom-modeline-info))))
  :config
  (doom-modeline-mode 1)

  :custom-face
  (doom-modeline ((t (:inherit mode-line))))
  (doom-modeline-bar ((t (:inherit doom-modeline))))
  (doom-modeline-bar-inactive ((t (:inherit mode-line-inactive))))
  (doom-modeline-buffer-file ((t (:inherit mode-line-buffer-id))))
  (doom-modeline-buffer-major-mode ((t nil)))
  (doom-modeline-buffer-minor-mode ((t (:inherit doom-modeline :slant normal :weight normal))))
  (doom-modeline-buffer-modified ((t (:inherit doom-modeline-buffer-path :foreground "#60f5ff"))))
  (doom-modeline-buffer-path ((t (:inherit mode-line-emphasis))))
  (doom-modeline-info ((t (:inherit doom-modeline))))
  (doom-modeline-notification ((t (:foreground "#41ff87"))))
  (doom-modeline-project-dir ((t (:inherit doom-modeline))))
  (doom-modeline-project-name ((t (:inherit (doom-modeline italic)))))
  (doom-modeline-project-parent-dir ((t (:inherit doom-modeline))))
  (doom-modeline-project-root-dir ((t (:inherit doom-modeline))))
  (doom-modeline-warning ((t (:inherit (doom-modeline warning)))))
  (doom-modeline-workspace-name ((t nil))))

(use-package minions
  :config
  (add-to-list 'minions-prominent-modes 'atomic-chrome-edit-mode)
  (minions-mode 1))

(column-number-mode 1)

(provide 'init-modeline)
;;; init-modeline.el ends here
