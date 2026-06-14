;;; -*- lexical-binding: t -*-

;; Copyright © 2018, 2020, 2022-2025 Michael Shields
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

(tree-sitter-langs-install-latest-grammar t)

;; The tree-sitter-langs package installs many useful grammars as .dylib files,
;; but it does not name them in the way that Emacs expects to find them.
(make-directory shields/tree-sitter-langs-path t)
(let ((source-dir (straight--build-dir "tree-sitter-langs" "bin")))
  (dolist (file (directory-files source-dir nil "\\.\\(dylib\\|so\\)$"))
    (make-symbolic-link (expand-file-name file source-dir)
                        (expand-file-name (concat "libtree-sitter-" file)
                                          shields/tree-sitter-langs-path)
                        t)))

;; As of 2025-04-21, tree-sitter-langs-grammars uses an ancient Lua grammar that
;; doesn't work at all.  Inelegantly replace it.
(add-to-list 'treesit-language-source-alist
             '(lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua")))
(treesit-install-language-grammar 'lua)
(delete-file (expand-file-name "libtree-sitter-lua.dylib" shields/tree-sitter-langs-path))
