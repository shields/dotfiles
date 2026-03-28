;;; init-search.el --- Search and documentation lookup -*- lexical-binding: t -*-

;; Copyright © 2020 Michael Shields
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
