(global-completion-preview-mode 1)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-count 20
        vertico-cycle t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 86400)) ; 24 hours in seconds

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
