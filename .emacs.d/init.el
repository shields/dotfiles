;;; .emacs.d/init.el --- Shields's Emacs initialization file

;; Author: Michael Shields <shields@msrl.com>

;;; Globals
;;{{{ Preliminaries

(when (eq system-type 'darwin)
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Write customizations to a separate file instead of appending here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;}}}

;; TODO: Organize these legacy imports
(use-package anzu)
(use-package apheleia)
(use-package avy)
(use-package cape)
(use-package crux)
(use-package dap-mode)
(use-package dash-at-point)
(use-package dockerfile-mode)
(use-package doom-modeline)
(use-package eterm-256color)
(use-package flx)
(use-package go-mode)
(use-package goto-last-change)
(use-package hl-todo)
(use-package jsonnet-mode)
(use-package magit)
(use-package magit-delta)
(use-package markdown-mode)
(use-package minions)
(use-package multi-term)
(use-package posframe)
(use-package ruff-format)
(use-package smartparens)
(use-package symbol-overlay)
(use-package terraform-mode)
(use-package typo)
(use-package yaml-mode)
(use-package yasnippet)

;;{{{ Customization of commands

;; Disabled commands.  Hmmph.
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;; Stop saying "You can run the command blah-blah with M-x bl-b".
(setq extended-command-suggest-shorter nil)

;;}}}
;;{{{ Display

(setq inhibit-startup-message t)

(setq initial-scratch-message nil)

;; Don't close windows because they're too short.  Sometimes a
;; single-line (plus mode line) window can be useful.
(setq window-min-height 2)

;; Highlight tabs and trailing spaces.
(setq-default whitespace-style
              '(face
                tabs trailing space-before-tab space-after-tab tab-mark
                missing-newline-at-eof))
(global-whitespace-mode 1)
(defun shields/suppress-whitespace-mode ()
  (setq-local whitespace-style nil))

(blink-cursor-mode 0)

(setq blink-matching-delay 0.25)

(menu-bar-mode 0)
(tool-bar-mode 0)

(setq tab-bar-show 1)
(setq tab-bar-close-last-tab-choice 'delete-frame)

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
       (setq visible-bell nil)
       (setq ring-bell-function 'macos-system-alert))
      (t
       (setq visible-bell t)))

(set-fringe-mode '(nil . 0))            ; left-only

(add-to-list 'default-frame-alist '(height . 999))
(add-to-list 'default-frame-alist '(width . 132))

;; Enable color emoji.
(set-fontset-font
 t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))
(eval-after-load "magit"
  '(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Enable smartparens.  Note that it requires configuration, and that
;; a stock configuration is provided by smartparens-config.  If you
;; just let it autoload, it will work, but not well.
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(setq sp-show-pair-delay 0)
(setq sp-ignore-modes-list nil)                 ; Even the minibuffer!

(global-hl-todo-mode 1)
(setq hl-todo-keyword-faces
      '(("FIXME" . "#ff0000")
        ("XXX+"  . "#ff0000")))

(eval-after-load "symbol-overlay"
  '(face-spec-set 'symbol-overlay-default-face
                  '((t :weight bold :inherit nil))))
(add-hook 'prog-mode-hook
          #'(lambda () (symbol-overlay-mode 1)))
(setq symbol-overlay-idle-time 0.1)

(setq scroll-error-top-bottom t)

(setq-default truncate-lines t)

;; Enable horizontal trackpad scrolling.
(setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)

;; Enable sub-line scrolling.
(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum t)

;;}}}
;;{{{ Editing behavior

(delete-selection-mode 1)

(add-hook 'prog-mode-hook
          #'(lambda () (subword-mode 1)))

(setq-default indent-tabs-mode nil)

(setq line-move-visual nil)

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
  (setq aggressive-indent-excluded-modes '(go-mode terraform-mode))
  (global-aggressive-indent-mode 1))

;; Leave electric-indent enabled for modes that don't work well with
;; aggressive-indent.
(electric-indent-mode 1)

(kill-ring-deindent-mode t)

(require 'apheleia)
(apheleia-global-mode 1)
;; Replace black with ruff, and gofmt with goimports.
(dolist (el apheleia-mode-alist)
  (when (eq (cdr el) 'black)
    (setf (cdr el) 'ruff))
  (when (eq (cdr el) 'gofmt)
    (setf (cdr el) 'goimports)))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(setq save-interprogram-paste-before-kill t)

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

(setq flymake-show-diagnostics-at-end-of-line t)

(which-key-mode 1)
(which-key-setup-minibuffer)

;;}}}
;;{{{ Mode line

(doom-modeline-mode 1)

(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-major-mode-icon nil)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-minor-modes t)
(setq doom-modeline-number-limit 999)
(setq doom-modeline-vcs-max-length 32)
(setq doom-modeline-column-zero-based nil)
(setq doom-modeline-total-line-number t)
(setq doom-modeline-position-column-line-format '("c%c %l"))

(minions-mode 1)

;;}}}
;;{{{ Files and buffers

;; Make sure editing a hard-linked file edits all its links.
(setq backup-by-copying-when-linked t)

;; From example in advice.el 2.14:
(defadvice switch-to-buffer (before existing-buffers-only activate)
  "When called interactively switch to existing buffers only, unless
when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;}}}
;;{{{ Global keybindings

;; Option (or Alt) ⌥: ignore so as to allow system-wide symbol input mechanism.
(setq ns-alternate-modifier nil)
(setq ns-right-alternate-modifier nil)
;; Command (or Cmd) ⌘
(setq ns-command-modifier 'meta)
(setq ns-right-command-modifier 'meta)
;; macOS only supports four modifiers, but we use Karabiner elements to map two
;; additional keys to F24, then set them to a non-repeating `C-x @ s', in Emacs
;; only, to emulate Super.
;;
;; Align with other standard macOS shortcuts.
;; https://support.apple.com/en-us/102650
(global-set-key [(meta x)] #'kill-region) ; !
(global-set-key [(super x)] #'execute-extended-command)
(global-set-key [(meta c)] #'copy-region-as-kill)
(global-set-key [(meta v)] #'yank)
(global-set-key [(meta z)] #'undo)
(global-set-key [(meta a)] #'mark-whole-buffer)
(global-set-key [(meta f)] #'isearch-forward)
(global-set-key [(meta o)] #'find-file)
(global-set-key [(meta t)] #'tab-bar-new-tab)
(global-set-key [(meta w)] #'tab-bar-close-tab)

(global-set-key [(home)] #'move-beginning-of-line)
(global-set-key [(end)] #'move-end-of-line)

(global-set-key [(control c) (F)] 'find-file-at-point)

(global-set-key [(meta n)] 'next-error)

(global-set-key [(control c) (d)] 'dictionary-search)

(global-set-key [(control h) (a)] 'counsel-apropos)

;; Not M-SPC because that's Spotlight or Alfred.
(global-set-key [(super space)] 'fixup-whitespace)

(global-set-key [(control k)] #'crux-smart-kill-line)

(global-set-key [(control backspace)] 'join-line)

(global-set-key [(super o)] #'crux-smart-open-line)

(global-set-key [(meta p)] 'multi-term-next)

(global-set-key [(meta \:)] 'comment-dwim)

(global-set-key [(meta h)] 'goto-last-change)

(global-set-key [(meta \')] 'next-multiframe-window)
(global-set-key [(meta \")] 'previous-multiframe-window)
(global-set-key [(control x) (o)] nil)

(global-set-key [(meta m)] 'magit-status)

(global-set-key [(meta v)] #'yank)
;; Break old C-v habits now that M-v is paste (yank).
(global-set-key [(control v)] nil)

(global-set-key [(super .)] #'dash-at-point)
(global-set-key [(meta .)] #'xref-find-definitions)
(global-set-key [(meta \,)] #'xref-pop-marker-stack)

(global-set-key [(meta /)] 'completion-at-point)

(global-set-key [(meta k)] #'avy-goto-char-timer)

(global-set-key [(meta f)] nil)

(global-set-key [(meta g)] #'grep)

(global-set-key [(meta t)] #'previous-buffer)
(global-set-key [(meta T)] #'next-buffer)
(global-set-key [(control t)] #'switch-to-buffer)
(global-set-key [(control x) (b)] nil)

;; Only useful on keyboards with arrow keys:
(global-set-key [(meta up)] #'move-line-up)
(global-set-key [(meta down)] #'move-line-down)

(global-set-key [(control w)] nil)

;; Put M-ESC (i.e., ESC ESC) back to the way it was when I learned
;; Emacs.  Apparently this changed in 1994.
(global-set-key "\e\e" #'eval-expression)

(global-set-key [(meta b)] #'shields/open-dwim)

(global-set-key [(meta s)] #'shields/save-dwim)

(global-set-key [(meta q)] nil)

(global-set-key [(meta r)] #'replace-string)

(global-set-key [s-mouse-1] 'ffap-at-mouse)

(global-set-key [(control c) (d)] #'crux-duplicate-current-line-or-region)

(global-set-key [(control c) (e)] #'crux-eval-and-replace)

;;}}}

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

(defun shields/open-dwim (arg)
  "Open in the current project if in a project, otherwise whatever."
  (interactive "P")
  (if (project-current)
      (project-find-file)
    (counsel-find-file arg)))

(defun shields/save-dwim (arg)
  "Save and do other things.

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
                (when (buffer-file-name)
                  (save-buffer)))))))
    ;; File was already saved.
    (when (magit-file-relative-name)
      (magit-stage-file buffer-file-name)
      (magit-diff-buffer-file))))

;;}}}

;; Minor modes
;;{{{ anzu

(global-anzu-mode 1)

;;}}}
;;{{{ avy

;; QGMLWY home row, ordered by finger strength, starting with left
;; because S-k is on the right.
(setq avy-keys '(?n ?a ?t ?e ?s ?o ?d ?h ?r ?i)) ; QGMLWY home row

(setq avy-background t)

(setq avy-lead-faces '(error error error error error error))

(defun shields/avy-handler (char)
  "Terminate avy on RET."
  (if (eq char ?\C-m)
      (throw 'done 'exit)
    (avy-handler-default char)))
(setq avy-handler-function #'shields/avy-handler)

;;}}}

;;; Major modes
;;{{{ c-mode

(defun shields/c-mode-setup ()
  (c-set-style "k&r")
  (setq c-basic-offset 4)
  (define-key c-mode-map "\C-c\C-c" 'compile))
(add-hook 'c-mode-hook #'shields/c-mode-setup)

(setq c-cleanup-list '(brace-else-brace defun-close-semi))

;;}}}
;;{{{ emacs-lisp-mode

(eval-after-load "elisp-mode"
  '(define-key emacs-lisp-mode-map [(meta return)] #'eval-last-sexp))

(defun shields/eval-expression-minibuffer-setup ()
  (insert "()")
  (backward-char)
  ;; Don't smartparen-pair on '.
  (sp-update-local-pairs '(:open "'" :close nil :actions nil)))
(add-hook 'eval-expression-minibuffer-setup-hook
          #'shields/eval-expression-minibuffer-setup)

;;}}}
;;{{{ go-mode

(setq godoc-at-point-function #'godoc-gogetdoc)

;; Eglot setup for Go
(add-hook 'go-mode-hook #'eglot-ensure)

;; Note: go-mode is excluded from aggressive-indent-mode in its configuration

(add-hook 'go-mode-hook #'shields/suppress-whitespace-mode)

;;}}}
;;{{{ help-mode

(setq help-window-select t)

(customize-set-variable
 'display-buffer-alist
 '(("\\*Help\\*" display-buffer-same-window)))

(define-key help-mode-map [(q)] #'previous-buffer)

;;}}}
;;{{{ makefile-mode

(add-hook 'makefile-mode-hook
          '(lambda ()
             (define-key makefile-mode-map "\C-c\C-c" 'compile)))

;;}}}
;;{{{ Magit

(eval-after-load "magit"
  '(magit-wip-mode 1))
(setq magit-no-confirm '(safe-with-wip))

(setq magit-save-repository-buffers 'dontask)

(setq magit-diff-refine-hunk 'all)

;; Bind "=" to git diff origin/main.
(eval-after-load "magit"
  '(define-key magit-mode-map "="
               (lambda ()
                 (interactive)
                 (magit-diff-range "origin/main"))))

;;}}}
;;{{{ term-mode

;; After changing these, run (multi-term-keystroke-setup).
(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)
        ("C-c C-e" . term-send-esc)
        ("C-m" . term-send-return)
        ("s-v" . term-paste)
        ("C-r" . term-send-reverse-search-history)
        ("M-." . comint-dynamic-complete)))
(setq term-unbind-key-list
      '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>" "C-r" "C-s" "C-t"))

(add-hook 'term-mode-hook #'eterm-256color-mode)

(setq term-suppress-hard-newline t)

;;}}}
;;{{{ Terraform

;; Note: terraform-mode is excluded from aggressive-indent-mode in its configuration

;;}}}
;;{{{ text-mode and indented-text-mode

(setq-default fill-column 80)

(setq sentence-end-double-space nil)

;; Enable auto-fill.
(add-hook 'text-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)))
(add-hook 'indented-text-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)))
(add-hook 'message-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)))
(add-hook 'xml-mode-hook
          #'(lambda ()
              (turn-on-auto-fill)))

;; Perl extension glues.  Not really like C; more like a Makefile.
(or (assoc "\\.xs$" auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.xs$" . indented-text-mode)))

;;}}}
;;{{{ view-mode

(add-hook 'view-mode-hook
          (function (lambda ()
                      ;; Bind a few view-mode keys to vi/less-like
                      ;; bindings.  Maybe these should be integrated
                      ;; into view.el?
                      (define-key view-mode-map "j" 'next-line)
                      (define-key view-mode-map "k" 'previous-line)
                      (define-key view-mode-map "^" 'beginning-of-line)
                      (define-key view-mode-map "$" 'end-of-line)
                      (define-key view-mode-map "G"
                                  '(lambda (arg)
                                     (interactive "P")
                                     (cond ((null arg) (goto-char (point-max)))
                                           ((numberp arg) (goto-line arg))
                                           (t (error
                                               "Must use numeric or no argument"))))))))
;; Hmm... worked in Emacs 18.
;;(define-key view-mode-map "%"
;;  '(lambda (arg)
;;     (interactive "P")
;;     (cond ((numberp arg)
;;          (if (and (>= arg 0) (<= arg 100))
;;               (goto-char (+ (point-min)
;;                             (/ (* (- (point-max) (point-min)) arg) 100))))
;;            (error "No such thing as %d%%" arg)))
;;         (t (error "Must use numeric argument")))))

(setq view-read-only t)

;;}}}
;;{{{ XML

(add-to-list 'auto-mode-alist '("\\.html$" . xml-mode))

(eval-after-load "psgml-mode"
  '(add-hook 'xml-mode-hook
             (function (lambda ()
                         (define-key xml-mode-map "'"
                                     '(lambda ()
                                        (interactive)
                                        (insert-string "&#8217;")))))))

;;}}}

;;; Features
;;{{{ Calc

(setq calc-group-char " ")
(setq calc-date-format '(YYY "-" MM "-" DD (" " hh ":" mm ":" ss)))
(setq calc-display-trail nil)

(setq math-additional-units
      '((fathom "6 * ft" "Fathom")
        (furlong "mi / 8" "Furlong")
        (fortnight "14 * day" "Fourteen nights")))

;;}}}
;;{{{ Calendar and friends

;; Use local (US Pacific) time, not my usual TZ, which is UTC.
(setq calendar-time-zone -480)
(setq calendar-standard-time-zone-name "PST")
(setq calendar-daylight-time-zone-name "PDT")
(setq calendar-daylight-savings-starts
      '(calendar-nth-named-day 2 0 3 year))
(setq calendar-daylight-savings-ends
      '(calendar-nth-named-day 1 0 11 year))
(setq calendar-daylight-time-offset 60)
(setq calendar-daylight-savings-starts-time 120)
(setq calendar-daylight-savings-ends-time 120)

(setq calendar-week-start-day 1)

(setq calendar-date-display-form '(year "-" (format "%02d-%02d"
                                                    (string-to-number month)
                                                    (string-to-number day))))
(setq calendar-time-display-form '(24-hours ":" minutes
                                            (if time-zone" ") time-zone))

(add-hook 'initial-calendar-window-hook 'mark-calendar-holidays)

;;}}}
;;{{{ Completion in code

(defun shields/prog-capf ()
  (cape-wrap-super #'eglot-completion-at-point))
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'shields/prog-capf
                              #'cape-file
                              #'cape-dabbrev))))



;;}}}
;;{{{ compilation and grep

(setq compilation-message-face 'default)

(setq compilation-always-kill t)

(setq compilation-scroll-output 'first-error)

(eval-after-load "grep"
  '(progn
     (grep-apply-setting 'grep-command
                         (concat "rg -nH --null --color=always --no-heading "
                                 "--max-columns-preview --max-columns=80 "))

     ;; TODO: Figure out why this has no effect.
     (grep-apply-setting 'grep-highlight-matches 'always)

     (grep-apply-setting 'grep-use-null-device nil)
     (grep-apply-setting 'grep-use-null-filename-separator t)))

;; Enable editing grep results
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;;}}}
;;{{{ DAP (https://emacs-lsp.github.io/dap-mode/)

(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(tooltip-mode 1)
(dap-ui-controls-mode 1)

(require 'dap-gdb-lldb)
(dap-gdb-lldb-setup)

(require 'dap-go)
(dap-go-setup)

(require 'dap-python)

;;}}}
;;{{{ Dired

;; Use GNU ls from Homebrew, not BSD ls.
(when (file-executable-p "/opt/homebrew/bin/gls")
  (setq insert-directory-program "/opt/homebrew/bin/gls"))

(setq dired-use-ls-dired t)

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

(with-eval-after-load 'flymake
  (advice-add 'flymake-make-diagnostic :filter-args #'shields/flymake-make-diagnostic-advice))

;;}}}
;;{{{ Flyspell

(require 'ispell)
(setq-default ispell-program-name "aspell")
(setq ispell-silently-savep t)
(setq ispell-extra-args '("-W" "3"))

(require 'flyspell)

;; Normally using (flyspell-mode-on) directly is deprecated in favor
;; of (flyspell-mode 1), which is smart enough not to reinitialize.
;; However, we actually want to reinitialize.  For example,
;; message-mode runs text-mode-hook before message-mode-hook; if
;; flyspell mode is already on, then flyspell-generic-check-word-p
;; will never get set with its message-mode-specific value.
(add-hook 'text-mode-hook 'flyspell-mode-on)
(add-hook 'message-mode-hook 'flyspell-mode-on)

;; flyspell-prog-mode depends on font-lock to identify comments and
;; strings, so it won't work without it anyway.
(add-hook 'font-lock-mode-hook 'flyspell-prog-mode)

(setq flyspell-abbrev-p nil)
(setq flyspell-sort-corrections nil)

(setq flyspell-persistent-highlight nil)

(define-key flyspell-mode-map [(meta tab)] nil)

;;}}}
;;{{{ Font-lock

(require 'font-lock)
(setq font-lock-face-attributes
      '((font-lock-comment-face "MidnightBlue")
        (font-lock-string-face "dark green")
        (font-lock-keyword-face "Purple")
        (font-lock-function-name-face "Blue")
        (font-lock-variable-name-face "Firebrick")
        (font-lock-type-face "DarkOliveGreen")
        (font-lock-reference-face "OrangeRed")))

(setq font-lock-maximum-size 2097152)

;;}}}
;;{{{ LLMs

(use-package gptel
  :config
  (setq gptel-model "gpt-4o")
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll))

(use-package chatgpt-shell
  :config
  (setq chatgpt-shell-anthropic-key (auth-source-pick-first-password :host "api.anthropic.com"))
  (setq chatgpt-shell-openai-key (auth-source-pick-first-password :host "api.openai.com")))

;;}}}
;;{{{ Completion

(load "~/.emacs.d/completion-config.el")

;;}}}
;;{{{ Info

(add-hook 'Info-mode-hook
          #'(lambda () (variable-pitch-mode 1)))

;;}}}
;;{{{ jka-compr

(jka-compr-install)

;;}}}
;;{{{ Eglot

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-display-functions '(eldoc-display-in-buffer))
  (eldoc-idle-delay 0.1)
  :config
  ;; Configure eldoc to display in side window
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  ;; Set up key bindings
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l d") #'eldoc)
  (define-key eglot-mode-map (kbd "C-c l h") #'eglot-help-at-point))

;;;}}}
;;{{{ Markdown

(add-hook 'markdown-mode-hook
          #'(lambda () (variable-pitch-mode 1)))

(add-hook 'markdown-mode-hook #'typo-mode)

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
;;{{{ Project

(setq project-mode-line t)

;;}}}
;;{{{ Python

(add-hook 'python-ts-mode-hook #'eglot-ensure)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("basedpyright-langserver" "--stdio"))))

;; Enable Ruff as an additional source of warnings. This calls
;; flymake-ruff-load for every Eglot mode, but it is a no-op for
;; non-Python modes.
(use-package flymake-ruff
  :hook (eglot-managed-mode . flymake-ruff-load))

;; "python" on macOS 10.15 is 2.7.
(setq python-shell-interpreter "python3")

;;}}}
;;{{{ Rust

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-?" . xref-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c a" . eglot-code-actions))
  :config
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  :custom
  (rustic-analyzer-command '("rust-analyzer"))
  (eglot-ignored-server-capabilities '(:inlayHintProvider)))

;;}}}
;;{{{ Swift

(use-package swift-mode
  :config
  (add-hook 'swift-mode-hook #'eglot-ensure))

;;}}}
;;{{{ TRAMP

(require 'tramp)

;;}}}
;;{{{ Version control

(require 'vc)

;; Default is "-c".
(setq diff-switches "-u")

;;}}}
;;{{{ W3

(setq url-keep-history nil)

;;}}}
;;{{{ yasnippet

;;(add-hook 'prog-mode-hook #'yas-minor-mode-on)

;;}}}

;;{{{ Aider

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  (setq aider-args nil)
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

(setq try-oblique-before-italic-fonts t)

(require 'edebug)

;; Modern performance optimizations
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-jit-compilation t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Increase process data chunks for better performance
(setq read-process-output-max (* 4 1024 1024)) ; 4MiB

;; File-name-handler-alist caching
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Persist history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode))

(server-start)

;; Don't block Emacs exit; that blocks automatic macOS upgrades.
(setq confirm-kill-processes nil)

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Andale Mono"))))
 '(Info-quoted ((t (:inherit fixed-pitch))))
 '(completions-annotations ((t (:inherit shadow))))
 '(corfu-preview ((t (:foreground "gray60"))))
 '(cperl-array-face ((t (:foreground "Blue"))))
 '(cperl-hash-face ((t (:foreground "Red" :weight bold))))
 '(cursor ((t (:background "firebrick"))))
 '(doom-modeline-bar ((t nil)))
 '(doom-modeline-bar-inactive ((t nil)))
 '(doom-modeline-buffer-file ((t (:inherit mode-line-buffer-id))))
 '(doom-modeline-buffer-major-mode ((t nil)))
 '(doom-modeline-buffer-minor-mode ((t (:slant normal))))
 '(doom-modeline-buffer-modified ((t (:inherit doom-modeline-buffer-path :foreground "blue"))))
 '(doom-modeline-buffer-path ((t (:inherit mode-line-emphasis))))
 '(doom-modeline-info ((t nil)))
 '(doom-modeline-project-dir ((t nil)))
 '(eglot-inlay-hint-face ((t (:inherit nil :background "gray97" :foreground "gray20" :height 0.707))))
 '(fixed-pitch ((t (:family "Andale Mono"))))
 '(flycheck-error ((t nil)))
 '(flycheck-info ((t nil)))
 '(flycheck-warning ((t nil)))
 '(flymake-end-of-line-diagnostics-face ((t (:background "gray97" :height 0.707))))
 '(flymake-error-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-error) :weight normal))))
 '(flymake-note-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-info) :weight normal))))
 '(flymake-warning-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-warning) :weight normal))))
 '(highlight ((t (:background "darkseagreen1"))))
 '(isearch ((t (:inherit match))))
 '(ivy-minibuffer-match-face-2 ((t (:foreground "dark magenta" :weight bold))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-3))))
 '(lazy-highlight ((t (:inherit match))))
 '(link-visited ((t (:inherit link))))
 '(eglot-highlight-symbol-face ((t (:background "#d0ffd0"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#f850f850f850" :height 0.8))))
 '(markdown-header-face ((t (:weight bold))))
 '(match ((t (:background "yellow1" :foreground "black"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(mode-line ((t (:background "grey90" :foreground "black" :box (:line-width -1 :style released-button) :family "Verdana"))))
 '(mode-line-buffer-id ((t (:weight semi-bold))))
 '(mode-line-emphasis ((t (:weight semi-bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey80" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
 '(show-paren-match ((t (:foreground "magenta" :weight bold))))
 '(show-paren-match-expression ((t (:background "#f4f4ff"))))
 '(sp-pair-overlay-face ((t (:inherit sp-show-pair-match-content-face))))
 '(sp-show-pair-match-content-face ((t (:inherit show-paren-match-expression))) t)
 '(sp-show-pair-match-face ((t (:inherit (show-paren-match show-paren-match-expression)))))
 '(sp-wrap-overlay-opening-pair ((t (:inherit sp-wrap-overlay-face :foreground "magenta"))))
 '(swiper-match-face-4 ((t (:inherit match))))
 '(variable-pitch ((t (:height 1.2 :family "Georgia"))))
 '(vertico-current ((t (:background "thistle1" :weight bold))))
 '(vertico-group-title ((t (:weight bold))))
 '(vhl/default-face ((t (:background "DarkSeaGreen1")))))

;;}}}
