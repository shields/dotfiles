;;; init-extras.el --- Miscellaneous utilities -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for various minor utilities and tools

;;; Code:

;; Misc tooling
(autoload 'rfcview-mode "rfcview")

(eval-after-load "fff"
  '(progn
     (setq fff-map-prefix [(control c) (f)])
     (fff-install-map)
     (require 'fff-rfc)
     (fff-rfc-install-map)
     (setq fff-rfc-view-mode 'rfcview-mode)
     ;; Debian doc-rfc-* packages use this directory:
     (add-to-list 'fff-rfc-path "/usr/share/doc/RFC/links")
     (require 'fff-elisp)
     (fff-elisp-install-map)))
(unless (featurep 'fff)
  (load "fff" t))

(provide 'init-extras)
;;; init-extras.el ends here
