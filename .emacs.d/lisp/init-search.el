;;; init-search.el --- Search and documentation lookup -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for grep, search, and documentation lookup

;;; Code:

(use-package dash-at-point
  :bind ("s-." . dash-at-point))

(use-package grep
  :config
  (grep-apply-setting 'grep-command
                      (concat "rg -nH --null --color=always --no-heading "
                              "--max-columns-preview --max-columns=132 "))
  (grep-apply-setting 'grep-use-null-device nil)
  (grep-apply-setting 'grep-use-null-filename-separator t))

;; Enable editing grep results
(use-package wgrep
  :after grep
  :custom
  (wgrep-auto-save-buffer t))

;; Web/URL handling
(use-package url
  :custom
  (url-keep-history nil))

(provide 'init-search)
;;; init-search.el ends here
