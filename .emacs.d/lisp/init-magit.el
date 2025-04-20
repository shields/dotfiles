;;; init-magit.el --- Magit configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for magit git interface

;;; Code:

(use-package magit
  :bind (("M-m" . magit-status)
         :map magit-mode-map
         ("=" . (lambda ()
                  (interactive)
                  (magit-diff-range "origin/main"))))
  :init
  (setq magit-define-global-key-bindings 'recommended)
  :custom
  (magit-no-confirm '(safe-with-wip))
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all)
  (magit-section-initial-visibility-alist
   '((file . show)
     (hunk . show)
     (commit . show)))
  :config
  (magit-wip-mode 1))

(use-package magit-delta)

(use-package forge)

(let ((github-username (magit-config-get-from-cached-list "github.user")))
  (when github-username
    (add-to-list 'forge-owned-accounts github-username)))

(use-package vc
  :custom
  (diff-switches "-u"))  ; Default is "-c"

(provide 'init-magit)
;;; init-magit.el ends here
