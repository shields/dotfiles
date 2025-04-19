;;; -*- lexical-binding: t -*-

;; Straight bootstrap, pasted from its README.md.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(use-package straight
  :custom
  (straight-use-package-by-default t)
  (straight-vc-git-default-clone-depth 1))

;; Pending https://debbugs.gnu.org/cgi/bugreport.cgi?bug=77928
(defun use-package-handler/:custom-face (name _keyword args rest state)
  "Generate use-package custom-face keyword code."
  (use-package-concat
   (mapcar #'(lambda (def)
               `(progn
                  (apply #'face-spec-set (append (backquote ,def) '(face-defface-spec)))
                  (put ',(car def) 'face-modified t)))
           args)
   (use-package-process-keywords name rest state)))

;; Work around https://github.com/joaotavora/eglot/discussions/1436
(straight-use-package 'project)
(require 'project)
(straight-use-package 'flymake)
(require 'flymake)

(setq use-package-always-ensure t)
