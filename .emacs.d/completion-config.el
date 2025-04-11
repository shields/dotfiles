(global-completion-preview-mode 1)

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20
        vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq orderless-matching-styles '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 86400)) ; 24 hours in seconds

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))
