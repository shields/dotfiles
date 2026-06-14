;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;; Copyright © 2003, 2020, 2022, 2024 Michael Shields
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
;; Configuration for directory editor and file management

;;; Code:

;; Dired configuration
(setopt dired-use-ls-dired t)

;; Use GNU ls from Homebrew, not BSD ls.
(when (file-executable-p "/opt/homebrew/bin/gls")
  (setopt insert-directory-program "/opt/homebrew/bin/gls"))

;; jka-compr for compressed files
(use-package jka-compr
  :config
  (jka-compr-install))

(provide 'init-dired)
;;; init-dired.el ends here
