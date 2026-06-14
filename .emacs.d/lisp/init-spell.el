;;; init-spell.el --- Spell checking -*- lexical-binding: t -*-

;; Copyright © 2003, 2020, 2024-2025 Michael Shields
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
