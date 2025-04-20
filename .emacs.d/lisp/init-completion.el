;;; init-completion.el --- Completion framework configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Settings for minibuffer completion and in-buffer completion

;;; Code:

(global-completion-preview-mode 1)

(use-package vertico
  :config
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t)

  :custom-face
  (vertico-current ((t (:background "thistle1" :weight bold))))
  (vertico-group-title ((t (:weight bold)))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :config
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 86400)) ; 24 hours in seconds

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package flx)

;; Programming mode configuration
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'shields/prog-capf
                              #'cape-file
                              #'cape-dabbrev))))

(defun shields/prog-capf ()
  (cape-wrap-super #'eglot-completion-at-point))

(provide 'init-completion)
;;; init-completion.el ends here
