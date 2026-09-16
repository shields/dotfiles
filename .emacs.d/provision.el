;;; -*- lexical-binding: t -*-

;; Copyright © 2018, 2020, 2022-2026 Michael Shields
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

;; emacs --batch --script .emacs.d/provision.el

(setq shields/add-lisp-dir-to-load-path nil)
(load-file ".emacs.d/early-init.el")
(add-to-list 'load-path ".emacs.d/lisp")
(load-file ".emacs.d/init.el")

(straight-pull-all)
(straight-check-all)
(straight-remove-unused-repos t)

;; Each built-in tree-sitter mode registers its grammar recipe, pinned to a
;; commit the mode supports, when its library loads.  Install them all, so that
;; `treesit-enabled-modes' can use every mode it knows.
(dolist (mode (delete-dups (mapcar #'cdr treesit-major-mode-remap-alist)))
  (autoload-do-load (symbol-function mode) mode))
;; typst-ts-mode is not built in, so it has no recipe.
(add-to-list 'treesit-language-source-alist
             '(typst "https://github.com/uben0/tree-sitter-typst"))
;; Unconditionally, because a new Emacs pins new grammar commits, and a grammar
;; older than the queries written against it fails in confusing ways.
(dolist (recipe treesit-language-source-alist)
  (treesit-install-language-grammar (car recipe)))
