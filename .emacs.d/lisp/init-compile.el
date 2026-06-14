;;; init-compile.el --- Compilation configuration -*- lexical-binding: t -*-

;; Copyright © 2020, 2024 Michael Shields
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
;; Configuration for compilation mode and build tools

;;; Code:

;; Compilation mode settings
(setopt compilation-message-face 'default)
(setopt compilation-always-kill t)
(setopt compilation-scroll-output 'first-error)

(use-package fancy-compilation)

(use-package compile
  :config
  (fancy-compilation-mode))

(provide 'init-compile)
;;; init-compile.el ends here
