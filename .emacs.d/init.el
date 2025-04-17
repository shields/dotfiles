;;; .emacs.d/init.el --- Shields's Emacs initialization file  -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Globals
;;{{{ Preliminaries

(when (eq system-type 'darwin)
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Write customizations to a separate file instead of appending here.
(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;;}}}


;;{{{ Customization of commands

;; Disabled commands.  Hmmph.
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;; Stop saying "You can run the command blah-blah with M-x bl-b".
(setopt extended-command-suggest-shorter nil)

;;}}}
;;{{{ Display

(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
  (setopt hl-todo-keyword-faces
          '(("FIXME" . "#ff0000")
            ("XXX+"  . "#ff0000"))))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (setopt symbol-overlay-idle-time 0.1))

;; Basic interface settings
(setopt inhibit-startup-message t
        initial-scratch-message nil
        window-min-height 2
        blink-matching-delay 0.25
        tab-bar-show 1
        tab-bar-close-last-tab-choice 'delete-frame
        use-dialog-box nil)

;; Disable various visual elements
(blink-cursor-mode 0)
(menu-bar-mode (not (eq system-type 'darwin)))
(tool-bar-mode 0)

;; Highlight tabs and trailing spaces
(setq-default whitespace-style
              '(face
                tabs trailing space-before-tab space-after-tab tab-mark
                missing-newline-at-eof))
(global-whitespace-mode 1)
(defun shields/suppress-whitespace-mode ()
  (setq-local whitespace-style nil))

;; Remove trailing whitespace on save, for edited lines only.
(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

;; Enable visual bell.  But on macOS, the visual bell pops up "the
;; standard NextStep image 'caution'" (src/nsterm.m).  This is not
;; correct.  Better is to set "Flash the screen when an alert sound
;; occurs" in Accessibility preferences.
(defun macos-system-alert ()
  "Make the systemwide alert event (sound or screen flash)."
  (do-applescript "tell application \"System Events\" to beep"))
;; This needs to check system-type and not window-system because an Emacs daemon
;; started as a macOS login item is headless.
(cond ((eq system-type 'darwin)
       (setopt visible-bell nil)
       (setopt ring-bell-function 'macos-system-alert))
      (t
       (setopt visible-bell t)))

(set-fringe-mode '(nil . 0))            ; left-only

(setf (alist-get 'height default-frame-alist) 999)
(setf (alist-get 'width default-frame-alist) 132)
(setf (alist-get 'internal-border-width default-frame-alist) 0)

;; Enable color emoji.
(set-fontset-font
 t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; Enable smartparens.  Note that it requires configuration, and that
;; a stock configuration is provided by smartparens-config.  If you
;; just let it autoload, it will work, but not well.
(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setopt sp-show-pair-delay 0
          sp-ignore-modes-list nil))              ; Even the minibuffer!

(setopt scroll-error-top-bottom t)

(setq-default truncate-lines t)

;; Enable horizontal trackpad scrolling.
(setopt mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t)

;; Enable sub-line scrolling and window sizing.
(pixel-scroll-precision-mode t)
(setopt pixel-scroll-precision-use-momentum t)
(setopt frame-resize-pixelwise t)

(setopt switch-to-buffer-obey-display-actions t)

;;}}}
;;{{{ Editing behavior

(use-package cape)

(use-package crux
  :bind
  (("C-k" . crux-smart-kill-line)
   ("s-o" . crux-smart-open-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c e" . crux-eval-and-replace)))

(use-package goto-chg
  :bind
  (("C-." . goto-last-change)
   ("C-," . goto-last-change-reverse)))

;; Basic editing settings
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setopt line-move-visual nil)
(setopt shift-select-mode nil)
(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)

(add-hook 'prog-mode-hook #'kill-ring-deindent-mode)

(global-so-long-mode 1)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode)
  (define-key whole-line-or-region-local-mode-map [remap comment-dwim] nil))

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
    (when (eq (cdr el) 'black)
      (setf (cdr el) 'ruff))
    (when (eq (cdr el) 'gofmt)
      (setf (cdr el) 'goimports))))

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

;;}}}
;;{{{ Mode line

(use-package doom-modeline
  :custom
  (doom-modeline-height 21)

  (doom-modeline-buffer-encoding nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-minor-modes t)
  (doom-modeline-number-limit 999)
  (doom-modeline-vcs-max-length 32)
  (doom-modeline-total-line-number t)
  (doom-modeline-position-column-line-format '("%c:%l"))
  :config
  (doom-modeline-mode 1))

(use-package minions
  :config
  (minions-mode 1))

(column-number-mode 1)

;;}}}
;;{{{ Files and buffers

;; Make sure editing a hard-linked file edits all its links.
(setopt backup-by-copying-when-linked t)

;; Only switch to existing buffers interactively
(defun shields/switch-to-buffer-existing-only (orig-fun &rest args)
  "When called interactively switch to existing buffers only, unless
when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg))))
  (apply orig-fun args))

(advice-add 'switch-to-buffer :around #'shields/switch-to-buffer-existing-only)

(setopt auto-save-default nil)
(setopt make-backup-files nil)
(setopt create-lockfiles nil)

(global-auto-revert-mode 1)
(setopt global-auto-revert-non-file-buffers t)
(setopt auto-revert-verbose nil)

;; This is only an issue when looking at the source for Emacs Lisp packages
;; installed via straight.
(setopt vc-follow-symlinks t)

;;}}}
;;{{{ Global keybindings

(defun shields/delete-window-or-frame ()
  "Call `delete-window'. If it fails, call `delete-frame'."
  (interactive)
  (condition-case nil
      (delete-window)
    (error (delete-frame))))

;; macOS modifier key setup
;; Option (or Alt) ⌥: ignore so as to allow system-wide symbol input mechanism
(setopt ns-alternate-modifier nil)
(setopt ns-right-alternate-modifier nil)
;; Command (or Cmd) ⌘
(setopt ns-command-modifier 'meta)
(setopt ns-right-command-modifier 'meta)

;; macOS only supports four modifiers, but we use Karabiner elements to map two
;; additional keys to F24, then set them to a non-repeating `C-x @ s', in Emacs
;; only, to emulate Super.

;; Standard macOS shortcuts - https://support.apple.com/en-us/102650
;; See also ns-win.el
(global-set-key [(meta x)] #'kill-region)
(global-set-key [(super x)] #'execute-extended-command)
(global-set-key [(meta c)] #'copy-region-as-kill)
(global-set-key [(meta v)] #'yank)
(global-set-key [(meta z)] #'undo)
(global-set-key [(meta a)] #'mark-whole-buffer)
(global-set-key [(meta f)] #'isearch-forward)
(global-set-key [(meta o)] #'find-file)
(global-set-key [(meta w)] #'shields/delete-window-or-frame)
(with-eval-after-load 'isearch
  (define-key isearch-mode-map [(meta g)] #'isearch-repeat-forward))

;; Navigation
(global-set-key [(meta \`)] #'other-frame)
(global-set-key [(meta \')] #'next-multiframe-window)
(global-set-key [(meta \")] #'previous-multiframe-window)
(global-set-key [(meta n)] #'next-error)
(global-set-key [(meta p)] #'previous-error)
(global-set-key [(meta t)] #'previous-buffer)
(global-set-key [(meta T)] #'next-buffer)
(global-set-key [(control t)] #'switch-to-buffer)

;; Coding
(global-set-key [(meta /)] #'completion-at-point)
(global-set-key [(meta \:)] #'comment-dwim)
(global-set-key [(control c) (F)] #'find-file-at-point)
(global-set-key [(super space)] #'fixup-whitespace)
(global-set-key [(control backspace)] #'join-line)
(global-set-key [(meta g)] #'grep)
(global-set-key [(meta r)] #'replace-string)
(global-set-key [s-mouse-1] #'ffap-at-mouse)

;; Put M-ESC (i.e., ESC ESC) back to the way it was when I learned
;; Emacs.  Apparently this changed in 1994.
(global-set-key "\e\e" #'eval-expression)

;; Disable some keys
(global-unset-key [(control x) (f)])  ; set-fill-column
(global-unset-key [(control x) (o)])  ; other-window
(global-unset-key [(control v)])      ; scroll-up-command
(global-unset-key [(control w)])      ; kill-region
(global-unset-key [(meta q)])         ; macOS standard to quit
(global-unset-key [(super t)])        ; menu-set-font

;;}}}

(use-package xref
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-pop-marker-stack)))

;;{{{ New commands

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key [(meta up)] #'move-line-up)
(global-set-key [(meta down)] #'move-line-down)

(use-package project
  :bind ("M-b" . shields/open-dwim)
  :custom
  (project-mode-line t))

(defun shields/open-dwim (arg)
  "Open in the current project if in a project, otherwise whatever."
  (interactive "P")
  (if (project-current)
      (project-find-file)
    (counsel-find-file arg)))

(defun shields/save-dwim (arg)
  "Save and do other things.

If the file is part of an active server edit or `with-editor' session,
then finish that.

If the file is being freshly saved and it is part of a project,
also save all other project buffers.

If the file was already saved and it is part of a Magit repo,
stage it and display a diff."
  (interactive "P")
  (if (buffer-modified-p)
      ;; File is being freshly saved.
      (progn
        (save-buffer)
        (when-let* ((proj (project-current)))
          (let ((inhibit-message t))
            (dolist (buf (project-buffers proj))
              (with-current-buffer buf
                (when (and (buffer-file-name)
                           (buffer-modified-p))
                  (save-buffer)))))))
    ;; File was already saved. If Magit tracks it, then stage it.
    (when (and (buffer-file-name)
               (magit-file-tracked-p (buffer-file-name)))
      (magit-file-stage)
      (magit-diff-buffer-file)))
  ;; Finish server edit if applicable, whether or not we saved any
  ;; modifications.
  (cond (with-editor-mode
         (with-editor-finish arg))
        (server-buffer-clients
         (server-edit))))

;;}}}

;; Minor modes
;;{{{ anzu

(use-package anzu)
(global-anzu-mode 1)

;;}}}
;;{{{ avy

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

  (avy-handler-function #'shields/avy-handler))

;;}}}

;;; Major modes
;;{{{ C, C++, and Objective-C

(use-package c-ts-mode
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure))

(use-package cc-mode
  :hook
  (objc-mode . eglot-ensure))

;; Use a current clangd, not the old one that comes with Xcode.
(with-eval-after-load 'eglot
  (setf (alist-get '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                   eglot-server-programs)
        '("/opt/homebrew/opt/llvm/bin/clangd")))

;;}}}
;;{{{ dockerfile-mode

(use-package dockerfile-ts-mode)

;;}}}
;;{{{ emacs-lisp-mode

(use-package paren-face
  :hook emacs-lisp-mode)

(define-key emacs-lisp-mode-map [(meta return)] #'eval-last-sexp)

(defun shields/eval-expression-minibuffer-setup ()
  (insert "()")
  (backward-char)
  ;; Don't smartparen-pair on '.
  (sp-update-local-pairs '(:open "'" :close nil :actions nil)))

(add-hook 'eval-expression-minibuffer-setup-hook
          #'shields/eval-expression-minibuffer-setup)

(defun shields/elisp-eldoc-with-value ()
  (remove-hook 'eldoc-documentation-functions
               #'elisp-eldoc-var-docstring t)
  (add-hook 'eldoc-documentation-functions
            #'elisp-eldoc-var-docstring-with-value nil t))
(add-hook 'emacs-lisp-mode-hook #'shields/elisp-eldoc-with-value)

;; If we have a copy of the Emacs source code, `describe-function' can browse
;; into it.
(let ((dir "~/src/emacs/src"))
  (when (file-readable-p (expand-file-name "emacs.c" dir))
    (setq find-function-C-source-directory dir)))

;;}}}
;;{{{ Go

(use-package go-ts-mode
  :custom
  (godoc-at-point-function #'godoc-gogetdoc)
  :hook
  (go-ts-mode . eglot-ensure)
  (go-ts-mode . shields/suppress-whitespace-mode)
  (go-mod-ts-mode . shields/suppress-whitespace-mode)
  :config
  (setf (alist-get 'go-dot-mod-mode major-mode-remap-alist) #'go-mod-ts-mode))

;;}}}
;;{{{ help-mode

;; Help mode configuration
(setopt help-window-select t)

(add-to-list 'display-buffer-alist
             '("^\\*Help\\*"
               (display-buffer-reuse-mode-window
                display-buffer-below-selected)))

(define-key help-mode-map [(q)] #'quit-window)

;;}}}
;;{{{ jsonnet-mode

(use-package jsonnet-mode)

;;}}}
;;{{{ makefile-mode

;; Make-mode configuration
(add-hook 'makefile-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-c" #'compile)))

;;}}}
;;{{{ Magit

(use-package magit
  :bind (("M-m" . magit-status)
         ("M-s" . shields/save-dwim)
         :map magit-mode-map
         ("=" . (lambda ()
                  (interactive)
                  (magit-diff-range "origin/main"))))
  :init
  (setq magit-define-global-key-bindings 'recommended)
  :custom
  (magit-no-confirm '(safe-with-wip))
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all)
  :config
  (magit-wip-mode 1))

(use-package magit-delta)

;;}}}
;;{{{ sh and bash

(use-package sh-script
  :hook
  (bash-ts-mode . eglot-ensure))

(setf (alist-get 'sh-mode major-mode-remap-alist) #'bash-ts-mode)

;;}}}
;;{{{ term-mode

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package multi-term
  :bind ("M-p" . multi-term-next)
  :custom
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-m" . term-send-return)
     ("s-v" . term-paste)
     ("C-r" . term-send-reverse-search-history)
     ("M-." . comint-dynamic-complete)))
  (term-unbind-key-list
   '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "C-r" "C-s" "C-t"))
  (term-suppress-hard-newline t))

;;}}}
;;{{{ Terraform

(use-package terraform-mode)
;; Note: terraform-mode is excluded from aggressive-indent-mode in its configuration

;;}}}
;;{{{ text-mode and indented-text-mode

(setopt fill-column 80)

(setopt sentence-end-double-space nil)

;; Enable auto-fill in various modes
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'indented-text-mode-hook #'turn-on-auto-fill)
(add-hook 'message-mode-hook #'turn-on-auto-fill)
(add-hook 'xml-mode-hook #'turn-on-auto-fill)

;; Perl extension glues.  Not really like C; more like a Makefile.
(or (assoc "\\.xs$" auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.xs$" . indented-text-mode)))

;;}}}
;;{{{ view-mode

(defun shields/goto-prefix-percent (arg)
  (interactive "NGoto percentage: ")
  (if (and (>= arg 0) (<= arg 100))
      (goto-char (+ (point-min)
                    (/ (* (- (point-max) (point-min)) arg) 100)))
    (error "No such thing as %d%%" arg)))

(use-package view
  :custom
  (view-read-only t)
  :bind
  (:map view-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("^" . beginning-of-line)
        ("$" . end-of-line)
        ("G" . goto-line)
        ("%" . shields/goto-prefix-percent)))

;;}}}
;;{{{ XML

(add-hook 'xml-mode-hook
          (lambda ()
            (define-key xml-mode-map "'"
                        (lambda ()
                          (interactive)
                          (insert-string "&#8217;")))))

;;}}}
;;{{{ yaml-mode

(use-package yaml-mode)

;;}}}
;;; Features
;;{{{ Calc

(setopt calc-group-char " ")
(setopt calc-date-format '(YYY "-" MM "-" DD (" " hh ":" mm ":" ss)))
(setopt calc-display-trail nil)

(setq math-additional-units
      '((fathom "6 * ft" "Fathom")
        (furlong "mi / 8" "Furlong")
        (fortnight "14 * day" "Fourteen nights")))

;;}}}
;;{{{ Calendar and friends

;; Use local (US Pacific) time, not my usual TZ, which is UTC.
(setopt calendar-time-zone -480)
(setopt calendar-standard-time-zone-name "PST")
(setopt calendar-daylight-time-zone-name "PDT")
(setopt calendar-daylight-savings-starts
        '(calendar-nth-named-day 2 0 3 year))
(setopt calendar-daylight-savings-ends
        '(calendar-nth-named-day 1 0 11 year))
(setopt calendar-daylight-time-offset 60)
(setopt calendar-daylight-savings-starts-time 120)
(setopt calendar-daylight-savings-ends-time 120)

(setopt calendar-week-start-day 1)

(setopt calendar-date-display-form '(year "-" (format "%02d-%02d"
                                                      (string-to-number month)
                                                      (string-to-number day))))
(setopt calendar-time-display-form '(24-hours ":" minutes
                                              (if time-zone" ") time-zone))

(add-hook 'initial-calendar-window-hook 'mark-calendar-holidays)

;;}}}
;;{{{ Completion in code

(use-package flx)

;; Programming mode configuration
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'shields/prog-capf
                              #'cape-file
                              #'cape-dabbrev))))

(defun shields/prog-capf ()
  (cape-wrap-super #'eglot-completion-at-point))



;;}}}
;;{{{ compilation and grep

(use-package dash-at-point
  :bind ("s-." . dash-at-point))

;; Compilation mode settings
(setopt compilation-message-face 'default)
(setopt compilation-always-kill t)
(setopt compilation-scroll-output 'first-error)

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

(use-package fancy-compilation)

(use-package compile
  :config
  (fancy-compilation-mode))

;;}}}
;;{{{ DAP (https://emacs-lsp.github.io/dap-mode/)

(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)

  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)

  (require 'dap-go)
  (dap-go-setup)

  (require 'dap-python))

;;}}}
;;{{{ Dired

(setopt dired-use-ls-dired t)

;; Use GNU ls from Homebrew, not BSD ls.
(when (file-executable-p "/opt/homebrew/bin/gls")
  (setopt insert-directory-program "/opt/homebrew/bin/gls"))

;;}}}
;;{{{ Flymake

(defun shields/clean-flymake-diagnostic-message (message)
  (cond
   ((string-match "^\\(?:based\\)?pyright \\[[^]]+\\]: \\(.*\\)" message)
    (match-string 1 message))
   ((string-match "^Ruff: [A-Z0-9]+ \\(.*\\)" message)
    (match-string 1 message))
   (t message)))

(defun shields/flymake-make-diagnostic-advice (args)
  (let* ((locus (nth 0 args))
         (beg (nth 1 args))
         (end (nth 2 args))
         (type (nth 3 args))
         (text (nth 4 args))
         (data (nth 5 args))
         (overlay-properties (nth 6 args))
         (cleaned-text (shields/clean-flymake-diagnostic-message text)))
    (list locus beg end type cleaned-text data overlay-properties)))

(ert-deftest shields/test-clean-flymake-diagnostic-message ()
  (should (string=
           (shields/clean-flymake-diagnostic-message "pyright [reportUnknownVariableType]: Type of \"i\" is unknown")
           "Type of \"i\" is unknown"))
  (should (string=
           (shields/clean-flymake-diagnostic-message "basedpyright [reportUnknownVariableType]: Type of \"j\" is unknown")
           "Type of \"j\" is unknown"))
  (should (string=
           (shields/clean-flymake-diagnostic-message "Ruff: F821 Undefined name `y`")
           "Undefined name `y`"))
  (should (string=
           (shields/clean-flymake-diagnostic-message "This is a normal message")
           "This is a normal message")))

(use-package flymake
  :config
  (advice-add 'flymake-make-diagnostic :filter-args #'shields/flymake-make-diagnostic-advice))

;;}}}
;;{{{ Flyspell

(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-silently-savep t)
  (ispell-extra-args '("-W" "3")))

(use-package flyspell
  :custom
  (flyspell-abbrev-p nil)
  (flyspell-sort-corrections nil)       ; aspell already sorts
  :bind
  (:map flyspell-mode-map
        ([(meta tab)] . nil))
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . turn-on-flyspell))

;;}}}
;;{{{ LLMs

(use-package gptel
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  (gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (auth-source-pick-first-password :host "api.anthropic.com"))))

(use-package chatgpt-shell
  :config
  (setopt chatgpt-shell-anthropic-key (auth-source-pick-first-password :host "api.anthropic.com"))
  (setopt chatgpt-shell-openai-key (auth-source-pick-first-password :host "api.openai.com")))

;;}}}
;;{{{ Completion

(global-completion-preview-mode 1)

(use-package vertico
  :config
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-flex orderless-literal orderless-regexp)))

(use-package marginalia
  :config
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 86400)) ; 24 hours in seconds

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;;}}}
;;{{{ Info

(use-package info
  :hook (Info-mode . variable-pitch-mode))

(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node))

;;}}}
;;{{{ jka-compr

(use-package jka-compr
  :config
  (jka-compr-install))

;;}}}
;;{{{ Eldoc

(setopt eldoc-idle-delay 0.1)

;; Enable multiple documentation sources.
(setopt eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

;; Display in a "side" window at the bottom.
(setopt eldoc-display-functions '(eldoc-display-in-buffer))
(add-to-list 'display-buffer-alist
             '("^\\*eldoc"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.25)
               (window-parameters . ((no-delete-other-windows . t)))))

(global-eldoc-mode 1)

;;}}}
;;{{{ Eglot

(use-package eglot
  :config
  (setopt eglot-autoshutdown t)
  (setopt eglot-confirm-server-edits nil)
  :bind (:map eglot-mode-map
              ("C-c l r" . eglot-rename)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc)
              ("C-c l h" . eglot-help-at-point)
              ("C-c l i" . eglot-inlay-hints-mode)))

;; https://github.com/golang/tools/blob/master/gopls/doc/settings.md
(setq-default eglot-workspace-configuration
              '(:gopls ((staticcheck . t)
                        (vulncheck . "Imports")
                        (hints . ((assignVariableTypes . t)
                                  (compositeLiteralFields . t)
                                  (compositeLiteralTypes . t)
                                  (functionTypeParameters . t)
                                  (parameterNames . t)
                                  (rangeVariableTypes . t))))))

;;}}}
;;{{{ JavaScript and TypeScript

(use-package js
  :hook
  (js-ts-mode-hook . eglot-ensure))

(use-package typescript-ts-mode
  :hook
  (typescript-ts-mode-hook . eglot-ensure))

;;}}}
;;{{{ Markdown

(use-package markdown-mode
  :hook
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . typo-mode))

(use-package typo)

;;}}}
;;{{{ Perl modes

;; Make sure perl-mode doesn't even get loaded.
(defalias 'perl-mode 'cperl-mode)

(autoload 'describe-perl-symbol "perl-descr"
  "One-line information on a perl symbol" t)
(autoload 'switch-to-perl-doc-buffer "perl-descr"
  "One-line information on a perl symbol" t)

(eval-after-load "perl-mode"
  '(add-hook 'perl-mode-hook
             (function (lambda ()
                         (define-key perl-mode-map "\M-oq"
                                     'describe-perl-symbol)
                         (define-key perl-mode-map "\M-od"
                                     'switch-to-perl-doc-buffer)))))

(eval-after-load "cperl-mode"
  '(cperl-set-style "PerlStyle"))

;;}}}
;;{{{ Python

(use-package ruff-format)

;; Python configuration
;; "python" on macOS 10.15 is 2.7.
(setopt python-shell-interpreter "python3")

;; Python tree-sitter mode
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; Eglot configuration for Python
(with-eval-after-load 'eglot
  (setf (alist-get 'python-mode eglot-server-programs)
        '("basedpyright-langserver" "--stdio")))

;; Enable Ruff as an additional source of warnings. This calls
;; flymake-ruff-load for every Eglot mode, but it is a no-op for
;; non-Python modes.
(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

;;}}}
;;{{{ Rust

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :after rust-mode
  :bind (:map rustic-mode-map
              ("M-?" . xref-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c a" . eglot-code-actions))
  :custom
  (rustic-analyzer-command '("rust-analyzer"))
  (rustic-lsp-client 'eglot)
  :hook
  (rustic-mode . eglot-ensure))

;;}}}
;;{{{ Swift

(use-package swift-mode
  :hook (swift-mode . eglot-ensure))

;;}}}
;;{{{ Version control

(use-package vc
  :custom
  (diff-switches "-u"))  ; Default is "-c"

;;}}}
;;{{{ W3

(use-package url
  :custom
  (url-keep-history nil))

;;}}}
;;{{{ yasnippet

(use-package yasnippet
  ;; :hook (prog-mode . yas-minor-mode-on)
  )

;;}}}

;;{{{ Aider

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  (setopt aider-args nil)
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;}}}

;;{{{ Unsorted additions

(autoload 'rfcview-mode "rfcview")


(eval-after-load "fff"
  '(progn
     (setq fff-map-prefix [(control c) (f)])
     (fff-install-map)
     (require 'fff-rfc)
     (fff-rfc-install-map)
     (setq fff-rfc-view-mode 'rfcview-mode)
     ;; Debian doc-rfc-* packages use this directory:
     (add-to-list 'fff-rfc-path "/usr/share/doc/RFC/links")
     (require 'fff-elisp)
     (fff-elisp-install-map)))
(unless (featurep 'fff)
  (load "fff" t))


(eval-after-load "calc"
  '(progn
     (defun pop-up-calc ()
       "Create a new frame with Calc in it.
       The new frame has properties determined by calc-pop-up-frame-properties.
       This function is useful for binding to a hotkey."
       (interactive)
       (let ((frame (make-frame calc-pop-up-frame-properties))
             (buf (generate-new-buffer " pop-up-calc")))
         (select-frame frame)
         (focus-frame frame)
         (setq calc-transient-frames (cons frame calc-transient-frames))
         ;; If Calc starts up in its own buffer, it quits.  Hack around.
         (set-buffer buf)
         (full-calc)
         (kill-buffer buf)))
     (defvar calc-pop-up-frame-properties '(height 30 width 60)
       "Frame properties for frames created by \\[pop-up-calc].")
     (defvar calc-transient-frames ()
       "When calc-quit is run, the current frame will be deleted if it is in this list.")
     (defun calc-quit-or-delete-transient-frame (&optional non-fatal)
       "Deletes the current frame if it is a member of calc-transient-frames; otherwise, calc-quit."
       (interactive)
       (if (not (member (selected-frame) calc-transient-frames))
           (calc-quit non-fatal)
         (setq calc-transient-frames
               (delete (selected-frame) calc-transient-frames))
         (delete-frame)))
     (define-key calc-mode-map "q" 'calc-quit-or-delete-transient-frame)))


;; Enable the mouse.
(xterm-mouse-mode 1)

(require 'edebug)

;; Modern performance optimizations
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Native compilation settings
(when (featurep 'native-compile)
  (setopt native-comp-async-report-warnings-errors 'silent)
  (setopt native-comp-jit-compilation t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

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

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Andale Mono"))))
 '(Info-quoted ((t (:inherit fixed-pitch))))
 '(avy-goto-char-timer-face ((t (:inherit match))))
 '(avy-lead-face ((t (:background "magenta" :foreground "white"))))
 '(avy-lead-face-0 ((t (:inherit avy-lead-face :foreground "gray90"))))
 '(avy-lead-face-2 ((t (:inherit avy-lead-face :foreground "gray85"))))
 '(company-preview ((t (:foreground "gray60"))))
 '(completions-annotations ((t (:inherit shadow))))
 '(corfu-preview ((t (:foreground "gray60"))))
 '(cperl-array-face ((t (:foreground "Blue"))))
 '(cperl-hash-face ((t (:foreground "Red" :weight bold))))
 '(cursor ((t (:background "firebrick"))))
 '(doom-modeline ((t nil)))
 '(doom-modeline-bar ((t nil)))
 '(doom-modeline-bar-inactive ((t nil)))
 '(doom-modeline-buffer-file ((t (:inherit mode-line-buffer-id))))
 '(doom-modeline-buffer-major-mode ((t nil)))
 '(doom-modeline-buffer-minor-mode ((t (:slant normal))))
 '(doom-modeline-buffer-modified ((t (:inherit doom-modeline-buffer-path :foreground "#60f5ff"))))
 '(doom-modeline-buffer-path ((t (:inherit mode-line-emphasis))))
 '(doom-modeline-info ((t nil)))
 '(doom-modeline-notification ((t (:foreground "#41ff87"))))
 '(doom-modeline-project-dir ((t nil)))
 '(doom-modeline-warning ((t (:inherit (doom-modeline warning)))))
 '(eglot-highlight-symbol-face ((t nil)))
 '(eglot-inlay-hint-face ((t (:inherit nil :background "gray97" :foreground "gray20" :height 0.707))))
 '(fixed-pitch ((t (:family "Andale Mono"))))
 '(flycheck-error ((t nil)))
 '(flycheck-info ((t nil)))
 '(flycheck-warning ((t nil)))
 '(flymake-end-of-line-diagnostics-face ((t (:background "gray97" :height 0.707))))
 '(flymake-error-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-error) :weight normal))))
 '(flymake-note-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-info) :weight normal))))
 '(flymake-warning-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-warning) :weight normal))))
 '(font-lock-builtin-face ((t (:inherit font-lock-function-call-face))))
 '(font-lock-comment-face ((t (:foreground "#197019"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-markup-face ((t (:inherit font-lock-doc-face))))
 '(font-lock-function-call-face ((t (:foreground "#701919"))))
 '(font-lock-function-name-face ((t (:inherit font-lock-function-call-face :weight bold))))
 '(font-lock-keyword-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-negation-char-face ((t (:foreground "dark red"))))
 '(font-lock-property-name-face ((t (:inherit font-lock-property-use-face :weight bold))))
 '(font-lock-property-use-face ((t (:foreground "MidnightBlue"))))
 '(font-lock-punctuation-face ((t (:foreground "gray50"))))
 '(font-lock-reference-face ((t (:foreground "OrangeRed"))))
 '(font-lock-string-face ((t (:foreground "dark green"))))
 '(font-lock-type-face ((t (:foreground "#7070c2"))))
 '(font-lock-variable-name-face ((t (:weight bold))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face :weight normal))))
 '(font-lock-warning-face ((t (:inherit nil))))
 '(highlight ((t (:background "darkseagreen1"))))
 '(isearch ((t (:inherit match))))
 '(ivy-current-match ((t (:background "thistle1" :weight bold))))
 '(ivy-highlight-face ((t (:weight bold))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "dark magenta" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-3))))
 '(lazy-highlight ((t (:inherit match))))
 '(link-visited ((t (:inherit link))))
 '(lsp-face-highlight-textual ((t (:background "#d0ffd0"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#f850f850f850" :height 0.8))))
 '(markdown-header-face ((t (:weight bold))))
 '(match ((t (:background "yellow1" :foreground "black"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(mode-line ((t (:background "#005462" :foreground "white" :family "Avenir Next"))))
 '(mode-line-buffer-id ((t (:weight semi-bold))))
 '(mode-line-emphasis ((t (:weight semi-bold))))
 '(mode-line-highlight ((t (:background "#0093a9"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#484848"))))
 '(parenthesis ((t (:inherit font-lock-bracket-face))))
 '(show-paren-match ((t (:foreground "magenta" :weight bold))))
 '(show-paren-match-expression ((t (:background "#f4f4ff"))))
 '(sp-pair-overlay-face ((t (:inherit sp-show-pair-match-content-face))))
 '(sp-show-pair-match-content-face ((t (:inherit show-paren-match-expression))) t)
 '(sp-show-pair-match-face ((t (:inherit (show-paren-match show-paren-match-expression)))))
 '(sp-wrap-overlay-opening-pair ((t (:inherit sp-wrap-overlay-face :foreground "magenta"))))
 '(swiper-match-face-4 ((t (:inherit match))))
 '(symbol-overlay-default-face ((t (:foreground "magenta"))))
 '(variable-pitch ((t (:height 1.2 :family "Avenir Next"))))
 '(vertico-current ((t (:background "thistle1" :weight bold))))
 '(vertico-group-title ((t (:weight bold))))
 '(vhl/default-face ((t (:background "DarkSeaGreen1")))))

;;}}}
