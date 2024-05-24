;;; .emacs.d/init.el --- Shields's Emacs initialization file

;; Author: Michael Shields <shields@msrl.com>

;;; Globals
;;{{{ Preliminaries

(when (eq system-type 'darwin)
  (exec-path-from-shell-initialize))

;; Write customizations to a separate file instead of appending here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "~/.emacs.d/package-repos.el")

;;}}}
;;{{{ Temporary fixes

;; https://github.com/Malabarba/aggressive-indent-mode/issues/137
;; Closed 2020-05-12 but not fixed as requested.
(require 'aggressive-indent)
(defun aggressive-indent--indent-if-changed (buffer)
  "Indent any region that changed in BUFFER in the last command loop."
  (if (not (buffer-live-p buffer))
      (and aggressive-indent--idle-timer
           (cancel-timer aggressive-indent--idle-timer))
    (with-current-buffer buffer
      (when (and aggressive-indent-mode aggressive-indent--changed-list)
        (save-excursion
          (save-selected-window
            (aggressive-indent--while-no-input
              (aggressive-indent--process-changed-list-and-indent))))
        (when (timerp aggressive-indent--idle-timer)
          (cancel-timer aggressive-indent--idle-timer))))))

;;}}}
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

;; Enable full-screen.
(if (and (eq window-system 'ns)
         (not (memq (frame-parameter nil 'fullscreen)
                    '(fullscreen fullboth))))
    (toggle-frame-fullscreen))

;; Don't close windows because they're too short.  Sometimes a
;; single-line (plus mode line) window can be useful.
(setq window-min-height 2)

;; Highlight tabs and trailing spaces.
(setq-default whitespace-style
              '(face
                tabs trailing space-before-tab space-after-tab tab-mark
                missing-newline-at-eof))
(global-whitespace-mode 1)

(blink-cursor-mode 0)

(setq blink-matching-delay 0.25)

(menu-bar-mode 0)
(tool-bar-mode 0)

;; Enable visual bell.  But on macOS, the visual bell pops up "the
;; standard NextStep image 'caution'" (src/nsterm.m).  This is not
;; correct.  Better is to set "Flash the screen when an alert sound
;; occurs" in Accessibility preferences.
(defun macos-system-alert ()
  "Make the systemwide alert event (sound or screen flash)."
  (do-applescript "tell application \"System Events\" to beep"))
(cond ((eq window-system 'ns)
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

(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)
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

(global-aggressive-indent-mode 1)
;; Leave electric-indent enabled for modes that don't work well with
;; aggressive-indent.
(electric-indent-mode 1)

(require 'apheleia)
(apheleia-global-mode 1)
;; Replace black with ruff.
(dolist (el apheleia-mode-alist)
  (when (eq (cdr el) 'black)
    (setf (cdr el) 'ruff)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(setq save-interprogram-paste-before-kill t)

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

(global-set-key [(home)] #'move-beginning-of-line)
(global-set-key [(end)] #'move-end-of-line)

(global-set-key [(control c) (F)] 'find-file-at-point)

(global-set-key [(super n)] 'next-error)

(global-set-key [(control c) (d)] 'dictionary-search)

(global-set-key [(control h) (a)] 'counsel-apropos)

;; The default is just-one-space.
(global-set-key [(super space)] 'fixup-whitespace)

(global-set-key [(control k)] #'crux-smart-kill-line)

(global-set-key [(control backspace)] 'join-line)

(global-set-key [(super o)] #'crux-smart-open-line)

(global-set-key [(super p)] 'multi-term-next)

(global-set-key [(super \;)] 'comment-dwim)

(global-set-key [(super h)] 'goto-last-change)

(global-set-key [(super \')] 'next-multiframe-window)
(global-set-key [(super \")] 'previous-multiframe-window)
(global-set-key [(control x) (o)] nil)

(global-set-key [(super m)] 'magit-status)

(global-set-key [(super v)] #'yank)
;; Break old C-v / M-v habits now that S-v is paste (yank).
(global-set-key [(control v)] nil)

(global-set-key [(super .)] #'xref-find-definitions)
(global-set-key [(super \,)] #'xref-pop-marker-stack)

(global-set-key [(super /)] 'company-complete)

(global-set-key [(super k)] #'avy-goto-char-timer)

(global-set-key [(super f)] nil)

(global-set-key [(super g)] #'grep)

(global-set-key [(super t)] #'previous-buffer)
(global-set-key [(super T)] #'next-buffer)
(global-set-key [(control t)] #'counsel-switch-buffer)
(global-set-key [(control x) (b)] nil)

(global-set-key [(super up)] #'move-line-up)
(global-set-key [(super down)] #'move-line-down)

(global-set-key [(control w)] nil)

;; Put M-ESC (i.e., ESC ESC) back to the way it was when I learned
;; Emacs.  Apparently this changed in 1994.
(global-set-key "\e\e" #'eval-expression)

(global-set-key [(super b)] #'shields/open-dwim)
(global-set-key [(super s)] #'shields/save-dwim)

(global-set-key [(super q)] nil)

(global-set-key [(super r)] #'replace-string)

(global-set-key [s-mouse-1] 'ffap-at-mouse)

;; Option-shift-hyphen for em dash, same as macOS ordinary combo.
(global-set-key [(meta _)]
                '(lambda () (interactive) (insert "—")))

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
  (if (projectile-project-root)
      (projectile-find-file arg)
    (counsel-find-file arg)))

(defun shields/save-dwim (arg)
  "Save and do other things.

If the file is being freshly saved, and it is part of a
Projectile project, also save all other project buffers, then run
the tests, if any.

If the file was already saved, and it is part of a Magit repo,
stage it and display a diff."
  (interactive "P")
  (if (buffer-modified-p)
      ;; File is being freshly saved.
      (progn
        (save-buffer)
        (when (projectile-project-root)
          (let ((inhibit-message t))
            (projectile-save-project-buffers))
          (let ((compilation-read-command nil)
                (p-c-d (projectile-compilation-dir)))
            (cond ((projectile-test-command p-c-d)
                   (projectile-test-project arg))))))
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
  '(define-key emacs-lisp-mode-map [(super return)] #'eval-last-sexp))

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

;; LSP setup.  https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

;; aggressive-indent-mode interacts badly with LSP's reformatting.
(add-to-list 'aggressive-indent-excluded-modes 'go-mode)

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

;; terraform-mode doesn't indent quite correctly, and will even undo
;; changes made by format-on-save.
(add-to-list 'aggressive-indent-excluded-modes 'terraform-mode)

;;}}}
;;{{{ text-mode and indented-text-mode

(setq-default fill-column 80)

(setq sentence-end-double-space nil)

;; Enable auto-fill.
(add-hook 'text-mode-hook
          (function (lambda ()
                      (turn-on-auto-fill))))
(add-hook 'indented-text-mode-hook
          (function (lambda ()
                      (turn-on-auto-fill))))
(add-hook 'message-mode-hook
          (function (lambda ()
                      (turn-on-auto-fill))))
(add-hook 'xml-mode-hook
          (function (lambda ()
                      (turn-on-auto-fill))))

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
;;{{{ Company and Codeium

(load-file "~/.emacs.d/codeium.el")
(use-package codeium
  :init
  (defun codium-and-lsp-completion-at-point ()
    (cape-wrap-super #'codium-completion-at-point #'lsp-completion-at-point))
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions '(codium-and-lsp-completion-at-point))))

  :config
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))

(use-package company
  :defer 0.1
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 0

   company-frontends '(company-preview-frontend)))

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
;;{{{ Flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default flycheck-indication-mode nil)

;; }}}
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

(define-key flyspell-mode-map [(super tab)] nil)

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
;;{{{ gptel

(use-package gptel
  :config
  (setq gptel-model "gpt-4o"))

;;}}}
;;{{{ ivy, counsel, and swiper

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))

(setq ivy-wrap t)

(counsel-mode 1)

;;}}}
;;{{{ Info

(add-hook 'Info-mode-hook
          #'(lambda () (variable-pitch-mode 1)))

;;}}}
;;{{{ jka-compr

(jka-compr-install)

;;}}}
;;{{{ LSP

(setq lsp-completion-enable nil)        ; Using Codeium instead.

(setq lsp-auto-guess-root t)

(lsp-ui-mode 1)

(setq lsp-ui-peek-enable nil)

(setq lsp-ui-doc-position 'bottom)
;; This doesn't get updated properly.
;; https://github.com/emacs-lsp/lsp-ui/issues/369
(face-spec-set 'lsp-ui-doc-background
               '((t :background "#eeeeff")))

;;;}}}
;;{{{ Markdown

(add-hook 'markdown-mode-hook
          #'(lambda () (variable-pitch-mode 1)))

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
;;{{{ Projectile

(projectile-global-mode)

(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

(setq projectile-completion-system 'ivy)

;;}}}
;;{{{ Python

(add-hook 'python-mode-hook #'lsp-deferred)

;; "python" on macOS 10.15 is 2.7.
(setq python-shell-interpreter "python3")

;; https://beta.ruff.rs/docs/editor-integrations/#emacs-unofficial
(require 'flymake-ruff)
(add-hook 'python-mode-hook #'flymake-ruff-load)

;;}}}
;;{{{ Rust

;; https://robert.kra.hn/posts/rust-emacs-setup/
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)))
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


;;}}}
;;{{{ TRAMP
;; http://tramp.sourceforge.net

(require 'tramp)

(setq tramp-default-method "rsync")

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
(require 'mwheel)
(mwheel-install)


(setq try-oblique-before-italic-fonts t)


(require 'edebug)

;; Allow Emacs to burn up to 0.5% of RAM before running GC.  The
;; default in 26.3 is 800 kB (!).  Prelude sets this to 100 MB.  If
;; it's set too low, Emacs will pause often for GC; if it's set too
;; high, the pauses will be long.
(setq gc-cons-threshold
      (if (eq system-type 'darwin)
          (/ (string-to-number
              (replace-regexp-in-string
               "^hw\\.memsize: \\([0-9]+\\)\n$"
               "\\1"
               (shell-command-to-string "sysctl hw.memsize")))
             200)
        100000000))
;; Increase the amount of data read from subprocsses.  Recommended by
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))

(server-start)

;; Don't block Emacs exit; that blocks automatic macOS upgrades.
(setq confirm-kill-processes nil)

(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Andale Mono"))))
 '(Info-quoted ((t (:inherit fixed-pitch))))
 '(company-preview ((t (:foreground "gray60"))))
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
 '(fixed-pitch ((t (:family "Andale Mono"))))
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
 '(mode-line ((t (:background "grey90" :foreground "black" :box (:line-width -1 :style released-button) :family "Avenir Next"))))
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
 '(variable-pitch ((t (:height 1.2 :family "Avenir Next"))))
 '(vhl/default-face ((t (:background "DarkSeaGreen1")))))

;;}}}
