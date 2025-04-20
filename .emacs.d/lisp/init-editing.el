;;; init-editing.el --- Editing configuration -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Settings related to text editing, including indentation, selection, etc.

;;; Code:

;; Anzu mode for search/replace feedback
(use-package anzu)
(global-anzu-mode 1)

;; Avy mode for efficient navigation
(defun shields/avy-handler (char)
  "Terminate avy on RET."
  (if (eq char ?\C-m)
      (throw 'done 'exit)
    (avy-handler-default char)))

(use-package avy
  :bind
  ;; The left thumb is F22 in Bazecor, but that isn't recognized as a key event
  ;; by macOS Emacs, so we remap it to F19. This could be less dumb.
  ("<f19>" . avy-goto-char-timer)

  :custom
  ;; QGMLWY home row, ordered by finger strength, starting with the right
  ;; because the action key is on the left.
  (avy-keys '(?a ?n ?e ?t ?o ?s ?h ?d ?i ?r))

  (avy-background t)

  (avy-orders-alist '((avy-goto-char . avy-order-closest)
                      (avy-goto-char-2 . avy-order-closest)
                      (avy-goto-char-timer . avy-order-closest)
                      (avy-isearch . avy-order-closest)))

  (avy-handler-function #'shields/avy-handler)

  :custom-face
  (avy-goto-char-timer-face ((t (:inherit match))))
  (avy-lead-face ((t (:background "magenta" :foreground "white"))))
  (avy-lead-face-0 ((t (:inherit avy-lead-face :foreground "gray90"))))
  (avy-lead-face-2 ((t (:inherit avy-lead-face :foreground "gray85")))))

(use-package cape)

(use-package crux
  :bind
  (("C-k" . crux-smart-kill-line)
   ("s-o" . crux-smart-open-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c e" . crux-eval-and-replace)))

(use-package goto-chg
  :bind
  (("s-." . goto-last-change)
   ("s-," . goto-last-change-reverse)))

;; Basic editing settings
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setopt line-move-visual nil)
(setopt shift-select-mode nil)
(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)

(setopt search-nonincremental-instead nil)

(add-hook 'prog-mode-hook #'kill-ring-deindent-mode)

(global-so-long-mode 1)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode)
  (keymap-set whole-line-or-region-local-mode-map "<remap> <comment-dwim>" nil))

;; Enable subword-mode in programming modes
(add-hook 'prog-mode-hook #'subword-mode)

;; Try to have pointer follow cursor.  This doesn't currently work
;; (Emacs 26.3, macOS 10.15.4) because (frame-pointer-visible-p) often
;; returns t even when the pointer is clearly not visible.
;;
;; (defun shields/set-hidden-pointer-at-cursor ()
;;   "Place the pointer at the cursor when it is not being used.
;;
;; This commonly happens via `make-pointer-invisible' (t by default)."
;;   (when (not (frame-pointer-visible-p))
;;     (let ((pos (mouse-avoidance-point-position)))
;;       (message "set to " pos)
;;       (set-mouse-position (car pos) (cadr pos) (cddr pos)))))
;; (setq shields/set-hidden-pointer-at-cursor-timer
;;       (run-with-idle-timer 0.1 t #'shields/set-hidden-pointer-at-cursor))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'go-ts-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-ts-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'terraform-mode)
  (global-aggressive-indent-mode 1))

;; Leave electric-indent enabled for modes that don't work well with
;; aggressive-indent.
(electric-indent-mode 1)

(kill-ring-deindent-mode t)

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  ;; Replace black with ruff, and gofmt with goimports.
  (dolist (el apheleia-mode-alist)
    (pcase (cdr el)
      ('black (setf (cdr el) 'ruff))
      ('gofmt (setf (cdr el) 'goimports)))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package tree-sitter)

;; The tree-sitter-langs package installs many useful grammars as .dylib files,
;; but it does not name them in the way that Emacs expects to find them.
(setq shields/tree-sitter-langs-path
      (locate-user-emacs-file "tree-sitter-langs-grammars"))
(defun shields/symlink-tree-sitter-langs-grammars ()
  (make-directory shields/tree-sitter-langs-path t)
  (let ((source-dir (straight--build-dir "tree-sitter-langs" "bin")))
    (dolist (file (directory-files source-dir nil "\\.\\(dylib\\|so\\)$"))
      (make-symbolic-link (expand-file-name file source-dir)
                          (expand-file-name (concat "libtree-sitter-" file)
                                            shields/tree-sitter-langs-path)
                          t))))

(use-package tree-sitter-langs
  :config
  (shields/symlink-tree-sitter-langs-grammars)
  (add-to-list 'treesit-extra-load-path shields/tree-sitter-langs-path))

(setopt treesit-font-lock-level 4)

(setopt flymake-show-diagnostics-at-end-of-line nil)

(which-key-mode 1)
(which-key-setup-minibuffer)

(defun shields/atomic-chrome-create-file-strategy (url extension)
  "Create atomic-chrome temp files in empty directories, so that tools are not
confused by other nearby files."
  (if extension
      (make-temp-file "atomic-chrome-" t)
    'buffer))

(use-package atomic-chrome
  :demand t
  :straight (atomic-chrome
             :repo "KarimAziev/atomic-chrome"
             :type git
             :flavor nil
             :host github)
  :commands (atomic-chrome-start-server)
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-create-file-strategy #'shields/atomic-chrome-create-file-strategy)
  :config
  (atomic-chrome-start-server))

;; Unsorted additions
;; Modern performance optimizations
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Native compilation settings
(when (featurep 'native-compile)
  (setopt native-comp-async-report-warnings-errors 'silent)
  (setopt native-comp-jit-compilation t))

;; Increase process data chunks for better performance
(setopt read-process-output-max (* 4 1024 1024)) ; 4MiB

;; File-name-handler-alist caching
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Tree-sitter mode remapping. This should run after all other packages have
;; been loaded, or else we might not know about the modes.
(defun shields/remap-to-tree-sitter-modes ()
  "Find all major modes ending with -ts-mode and create remappings from their non-ts versions."
  (interactive)
  (let ((ts-modes (apropos-internal "-ts-mode$" 'commandp)))
    (dolist (ts-mode ts-modes)
      (let* ((ts-mode-name (symbol-name ts-mode))
             (base-mode-name (replace-regexp-in-string "-ts-mode$" "-mode" ts-mode-name))
             (base-mode (intern base-mode-name)))
        (when (fboundp base-mode)
          (setf (alist-get base-mode major-mode-remap-alist) ts-mode))))))
(shields/remap-to-tree-sitter-modes)

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

;; Don't block Emacs exit; that blocks automatic macOS upgrades.
(setopt confirm-kill-processes nil)

(provide 'init-editing)
;;; init-editing.el ends here
