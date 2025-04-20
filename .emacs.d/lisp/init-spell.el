;;; init-spell.el --- Spell checking -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for spell checking with ispell and flyspell

;;; Code:

;; Flyspell for spell checking
(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-silently-savep t)
  (ispell-extra-args '("-W" "3")))

(use-package flyspell
  :custom
  (flyspell-abbrev-p nil)
  (flyspell-sort-corrections nil)       ; aspell already sorts
  (flyspell-use-meta-tab nil)
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . turn-on-flyspell))

(provide 'init-spell)
;;; init-spell.el ends here
