;;; .xemacs/init.el --- Shields's Emacs initialization file

;; Author: Michael Shields <shields@msrl.com>

;;; Globals
;;{{{ Preliminaries

(unless (boundp 'hostname)
  (setq hostname (replace-in-string (shell-command-to-string "hostname")
				    "[\r\n]" "")))

;; XXX
(load "messagexmas")
(load "mm-util")

(when (file-accessible-directory-p "~/info")
  (eval-after-load "info" '(add-to-list 'Info-directory-list "~/info")))
(when (file-accessible-directory-p "~/share/xemacs/site-packages/info")
  (eval-after-load "info" '(add-to-list 'Info-directory-list
					"~/share/xemacs/site-packages/info")))

(set-input-mode (car (current-input-mode)) ; High bit is not meta
		(nth 1 (current-input-mode))
		0)
(setq hexl-iso "-iso")
(require 'x-compose)
(global-set-key "\C-x8" compose-map)

(when mm-emacs-mule
  ;; UTF-8 voodoo from Stephen J. Turnbull:
  (require 'un-define)
  (unless (emacs-version>= 21 5 6)
    (require 'mule-ucs-unicode "unicode"))
  (set-coding-category-system 'utf-8 'utf-8)
  (set-pathname-coding-system 'utf-8)
  (set-process-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  ;; To have "only" UTF-8 in autodetection, the following tries UTF-8,
  ;; and then reads the file as binary (which always succeeds)
  ;;(set-coding-priority-list '(utf-8 binary))
  ;; This doesn't wipe out other autodetection; instead, it rotates the
  ;; listed coding categories to the front, and otherwise preserves
  ;; order.
  (set-coding-priority-list '(utf-8)))
 

;; Protect against init.el borrowing by other users.
(when (string-equal (user-login-name) "shields")
  (setq user-mail-address "shields@msrl.com"))

;; Protect .saves-PID-HOST file.  Adapted from Mike Long <mikel@shore.net>.
(and (boundp 'auto-save-list-file-name)
     (not (null auto-save-list-file-name)) ; added by Tim Moore <tmoore@tembel.org>
     (progn
       (or (file-exists-p auto-save-list-file-name)
	   (write-region "" nil auto-save-list-file-name nil 'quiet))
       (set-file-modes auto-save-list-file-name 384)))

(autoload 'debian-changelog-mode "debian-changelog-mode" nil t)

;;}}}
;;{{{ Customization of commands

;; Disabled commands.  Hmmph.
(put 'eval-expression 'disabled nil)	; C-:
(put 'narrow-to-region 'disabled nil)	; C-x n n
;;(setq disabled-command-hook 'ignore)

;; Stop C-n from adding newlines.  C-f doesn't add anything.  C-p
;; doesn't.  It's weird.
(setq next-line-add-newlines nil)

(setq enable-recursive-minibuffers t)

;;}}}
;;{{{ Display

(setq inhibit-startup-message t)

(setq search-slow-speed 2400)
(setq search-slow-window-lines 3)

;; Don't close windows because they're too short.  Sometimes a
;; single-line (plus mode line) window can be useful.
(setq window-min-height 2)

(setq display-time-24hr-format t)

(when (eq (console-type) 'x)
  (set-face-background 'default "white")
  (set-face-background 'modeline "grey88")
  (setq frame-title-format '("" hostname ": %b")))

(setq blink-matching-delay 0.25)

(when (eq (console-type) 'x)
  (set-specifier menubar-visible-p nil))
(set-specifier default-toolbar-visible-p nil)
(set-specifier default-gutter-visible-p nil)
(setq progress-feedback-use-echo-area t)

(defun turn-on-font-lock-unless-binary ()
  (unless (eq 'binary (coding-system-name buffer-file-coding-system))
    (turn-on-font-lock)))
(add-hook 'find-file-hooks 'turn-on-font-lock-unless-binary)

;;}}}
;;{{{ Files and buffers

;; Make sure editing a hard-linked file edits all its links.
(setq backup-by-copying-when-linked t)

;; Open only one buffer per file, regardless of how things are linked.
(setq find-file-existing-other-name t)

;; Ask for confirmation before saving without a newline.
(setq require-final-newline 'maybe)

;; From example in advice.el 2.14:
(defadvice switch-to-buffer (before existing-buffers-only activate)
  "When called interactively switch to existing buffers only, unless
when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg)))))

;;}}}
;;{{{ Global keybindings

;; Make auto-indentation work when I just hit RET.
(global-set-key [(return)] 'newline-and-indent)

;; C-x M-, will search; M-, will repeat.
(global-set-key [(control x) (meta ,)] 'tags-search)
      
(global-set-key [(control c) (g)] 'goto-line)

(global-set-key [(control c) (F)] 'find-file-at-point)

(global-set-key [(control x) (control n)] 'next-error)

(global-set-key [(control c) (d)] 'dictionary-search)
;(remove-hook 'text-mode-hook 'dictionary-tooltip-mode) ; not functional yet

;; The default is just-one-space.
(global-set-key [(meta space)] 'fixup-whitespace)

(global-set-key [(control backspace)] 'join-line)

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
;;{{{ makefile-mode

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (define-key makefile-mode-map "\C-c\C-c" 'compile)))

;;}}}
;;{{{ shell-mode

(add-hook 'shell-mode-hook
	  '(lambda ()
	     (set-process-sentinel (get-buffer-process (current-buffer))
				   'kill-shell-buffer-after-normal-exit)))
(defun kill-shell-buffer-after-normal-exit (process reason)
  "Kill the shell buffer after its process exits normally.
If it exits abnormally, print a message instead, like the default
sentinel."
  (cond ((string= reason "finished\n")
	 (kill-buffer (process-buffer process)))
	((buffer-name (process-buffer process))
	 (insert-string (concat "Process "
				(process-name process)
				" " reason)
			(process-buffer process)))))

;;}}}
;;{{{ text-mode and indented-text-mode

(require 'filladapt)

;; Enable auto-fill.
(add-hook 'text-mode-hook
	  (function (lambda ()
		      (turn-on-auto-fill)
		      (filladapt-mode 1))))
(add-hook 'indented-text-mode-hook
	  (function (lambda ()
		      (turn-on-auto-fill)
		      (filladapt-mode 1))))
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
;;{{{ Ange-ftp / EFS

;; Use anonymous by default, under the assumption that other machines
;; will be listed in .netrc.  
(setq ange-ftp-default-user "anonymous")
(setq ange-ftp-generate-anonymous-password (or (getenv "MAILTO")
					       t))

(setq efs-use-passive-mode t)

;;}}}
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
;;{{{ Flyspell

(require 'ispell)
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell"))
(setq ispell-silently-savep t)
(setq ispell-extra-args '("-W" "3"))

(when (executable-find ispell-program-name)

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

  (define-key flyspell-mode-map [(meta tab)] nil))

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
;;{{{ jka-compr

(jka-compr-install)

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
;;{{{ Suspension

(add-hook 'suspend-hook 'resume-suspend-hook)

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

(setq browse-url-browser-function 'browse-url-mozilla)

;;}}}
;;{{{ W3M

(setq w3m-fill-column 70)

;;}}}

;;{{{ Unsorted additions

(require 'eval-expr)
(eval-expr-install)


(resize-minibuffer-mode)


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



(setq visible-bell 'top-bottom)


;; All ^M to go to the beginning of line in shell mode.  This lets the
;; status update of apt-get, scp, &c. work correctly.
(eval-after-load "comint"
  '(progn
     (when (or (featurep 'proc-filters)
	       (load "proc-filters" t))
       (setq-default comint-output-filter-functions
		     (cons 'proc-filter-shell-output-filter
			   comint-output-filter-functions)))))


;; Enable the mouse wheel.
(require 'mwheel)
(mwheel-install)



(setq try-oblique-before-italic-fonts t)


(require 'mmm-mode)
(setq mmm-global-mode 'maybe)


(require 'edebug)


;;}}}
