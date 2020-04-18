;;; .emacs.d/init.el --- Shields's Emacs initialization file

;; Author: Michael Shields <shields@msrl.com>

;;; Globals
;;{{{ Preliminaries

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "~/.emacs.d/package-repos.el")

;; Native macOS Emacs isn't invoked from a shell.
(when (eq window-system 'ns)
  (exec-path-from-shell-copy-env "PATH"))

;;}}}
;;{{{ Customization of commands

;; Disabled commands.  Hmmph.
(put 'eval-expression 'disabled nil)	; C-:
(put 'narrow-to-region 'disabled nil)	; C-x n n
;;(setq disabled-command-hook 'ignore)

;; Stop saying "You can run the command blah-blah with M-x bl-b".
(setq extended-command-suggest-shorter nil)

;;}}}
;;{{{ Display

(setq inhibit-startup-message t)

;; Enable full-screen.
(if (and (eq window-system 'ns)
	 (not (memq (frame-parameter nil 'fullscreen)
		    '(fullscreen fullboth))))
    (toggle-frame-fullscreen))

;; Don't close windows because they're too short.  Sometimes a
;; single-line (plus mode line) window can be useful.
(setq window-min-height 2)

(blink-cursor-mode 0)

(setq blink-matching-delay 0.25)

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

(set-fringe-mode '(nil . 0))		; left-only

(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)
(eval-after-load "magit"
  '(progn
     (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
     (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(column-number-mode)
(setq column-number-indicator-zero-based nil)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(setq sp-show-pair-delay 0)

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

;;}}}
;;{{{ Editing behavior

(delete-selection-mode 1)

(add-hook 'prog-mode-hook
	  #'(lambda () (subword-mode 1)))

(global-ethan-wspace-mode 1)

(setq kill-whole-line t)

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

;;}}}
;;{{{ Mode line

(doom-modeline-mode 1)

(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-major-mode-icon nil)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-minor-modes t)

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

;;}}}
;;{{{ Global keybindings

(global-set-key [(home)] #'move-beginning-of-line)
(global-set-key [(end)] #'move-end-of-line)

(global-set-key [(control c) (F)] 'find-file-at-point)

(global-set-key [(control x) (control n)] 'next-error)

(global-set-key [(control c) (d)] 'dictionary-search)

(global-set-key [(control h) (a)] 'apropos) ; not apropos-command

;; The default is just-one-space.
(global-set-key [(super space)] 'fixup-whitespace)

(global-set-key [(control backspace)] 'join-line)

(global-set-key [(super p)] 'multi-term-next)

(global-set-key [(super \;)] 'comment-dwim)

(global-set-key [(super t)] 'split-window-right)

(global-set-key [(super m)] 'magit-status)

(global-set-key [(super i)] 'ido-find-file)

(global-set-key [(super v)] #'xah-paste-or-paste-previous)
;; Break old C-v / M-v habits now that S-v is paste (yank).
(global-set-key [(control v)] nil)

(global-set-key [(super .)] #'xref-find-definitions)
(global-set-key [(super \,)] #'xref-pop-marker-stack)

(global-set-key [(super /)] 'company-complete)

(global-set-key [(super k)] #'avy-goto-char-timer)

(global-set-key [(control t)] #'ido-switch-buffer)

(global-set-key [(super up)] #'move-line-up)
(global-set-key [(super down)] #'move-line-down)

(global-set-key [(super w)] #'er/expand-region)
(global-set-key [(control w)] nil)

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

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

When `universal-argument' is called first with a number arg, paste that many times.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
Version 2017-07-25"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes ($i (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

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

(add-hook 'c-mode-hook '(lambda ()
			  (c-set-style "k&r")
			  (setq c-basic-offset 4)))
(add-hook 'c-mode-hook '(lambda ()
			  (define-key c-mode-map "\C-c\C-c" 'compile)))

(setq c-cleanup-list '(brace-else-brace defun-close-semi))

;;}}}
;;{{{ emacs-lisp-mode

(eval-after-load "elisp-mode"
  '(define-key emacs-lisp-mode-map [(super return)] #'eval-last-sexp))

;;}}}
;;{{{ go-mode

(setq godoc-at-point-function #'godoc-gogetdoc)

(add-hook 'go-mode-hook #'go-eldoc-setup)

(add-hook 'before-save-hook 'gofmt-before-save)

(setq gofmt-command "goimports")

(eval-after-load "go-mode"
  '(define-key go-mode-map [(super .)] #'godef-jump))

;;}}}
;;{{{ help-mode

(setq help-window-select t)

;; }}}
;;{{{ makefile-mode

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (define-key makefile-mode-map "\C-c\C-c" 'compile)))

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

;;}}}
;;{{{ Terraform

(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; terraform-mode doesn't indent quite correctly, and will even undo
;; changes made by format-on-save.
(add-to-list #'aggressive-indent-excluded-modes 'terraform-mode)

;;}}}
;;{{{ text-mode and indented-text-mode

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
;;	    (if (and (>= arg 0) (<= arg 100))
;;		 (goto-char (+ (point-min)
;;			       (/ (* (- (point-max) (point-min)) arg) 100))))
;;	      (error "No such thing as %d%%" arg)))
;;	   (t (error "Must use numeric argument")))))

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
;;{{{ Company

(global-company-mode 1)

(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)

;; https://oremacs.com/2017/12/27/company-numbers/
(defun ora-company-number ()
  "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (cl-find-if (lambda (s) (string-match re s))
                    company-candidates)
        (self-insert-command 1)
      (company-complete-number (string-to-number k)))))
(mapc
 (lambda (x)
   (define-key company-active-map (format "%d" x) #'ora-company-number))
 (number-sequence 1 9))

;;}}}
;;{{{ Flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

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

(define-key flyspell-mouse-map [(button3)] #'flyspell-correct-word)

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
;;{{{ ido

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; Disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(ido-vertical-mode 1)

;;}}}
;;{{{ Info

(add-hook 'Info-mode-hook
	  #'(lambda () (variable-pitch-mode 1)))

;;}}}
;;{{{ jka-compr

(jka-compr-install)

;;}}}
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
(setq projectile-mode-line
      '(:eval
	(if (file-remote-p default-directory)
	    " Proj"
	  (format " Proj[%s]"
		  (projectile-project-name)))))

;;}}}
;;{{{ Python

;; "python" on macOS 10.15 is 2.7.
(setq python-shell-interpreter "python3")

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
;;{{{ W3M

(setq w3m-fill-column 70)

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

(global-set-key [s-mouse-1] 'ffap-at-mouse)


(setq try-oblique-before-italic-fonts t)


(require 'edebug)


;;}}}
