;;; -*- lexical-binding: t -*-
;;
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

(nerd-icons-install-fonts 1)
