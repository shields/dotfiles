;;; .emacs --- Shields's Emacs initialization file

;; Author: Michael Shields <shields@msrl.com>
;; Version: $Id$

;;; Globals
;;{{{ Preliminaries

(unless (boundp 'hostname)
  (setq hostname (replace-in-string (shell-command-to-string "hostname")
				    "[\r\n]" "")))

;; Must elegantize this:
(add-to-list 'load-path (expand-file-name "~/share/xemacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/share/xemacs/site-lisp"))
(add-to-list 'load-path (expand-file-name "~/share/xemacs/site-packages/lisp"))
;; XXX XXX ugly
(add-to-list 'load-path (expand-file-name "~/share/xemacs/site-packages/lisp/gnus"))

;; XXX
(load "messagexmas")
(load "mm-util")

;; Reload things in my dir that are already loaded.  Essential for
;; replacing Gnus elements with those built from the development
;; snapshot version.
(mapcar '(lambda (x)
	   (let ((base (concat (expand-file-name "~/share/xemacs/site-lisp")
			       "/" (car x))))
	     (if (or (file-readable-p (concat base ".elc"))
		     (file-readable-p (concat base ".el")))
		 (load base))))
	load-history)
;; XXX XXX ugly, fix, ugly ugly XXX
(when (file-exists-p "~/share/xemacs/site-packages/lisp/gnus/netrc")
  (load "~/share/xemacs/site-packages/lisp/gnus/netrc")
  (mapcar '(lambda (x)
	     (let ((base (concat (expand-file-name "~/share/xemacs/site-packages/lisp/gnus")
				 "/" (car x))))
	       (if (or (file-readable-p (concat base ".elc"))
		       (file-readable-p (concat base ".el")))
		   (load base))))
	  load-history))

(when (file-accessible-directory-p "~/info")
  (eval-after-load "info" '(add-to-list 'Info-directory-list "~/info")))
(when (file-accessible-directory-p "~/share/xemacs/site-packages/info")
  (eval-after-load "info" '(add-to-list 'Info-directory-list
					"~/share/xemacs/site-packages/info")))

;; Enable correct handling of ISO8859-1, and the C-x 8 compose key.
(standard-display-european 1)
(set-input-mode (car (current-input-mode)) ; High bit is not meta
		(nth 1 (current-input-mode))
		0)
(setq hexl-iso "-iso")
(require 'x-compose)
(global-set-key "\C-x8" compose-map)

(set-default-coding-systems 'utf-8)
;;(prefer-coding-system 'utf-8)   ; broken in XEmacs 21.4.6
;; try to stumble around it:
(set-coding-priority-list '(utf-8))
(set-coding-category-system 'utf-8 'utf-8)

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

;; umask
;;(set-default-file-modes 448)  ; 0700

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

;;(setq visible-bell nil)

;; Don't close windows because they're too short.  Sometimes a
;; single-line (plus mode line) window can be useful.
(setq window-min-height 2)

(setq display-time-24hr-format t)

(when (eq (console-type) 'x)
  (set-face-background 'default "white")
  (set-face-background 'modeline "grey88")
  (setq frame-title-format '("" hostname ": %b")))

(setq blink-matching-delay 0.25)

(set-specifier menubar-visible-p nil)
(set-specifier default-toolbar-visible-p nil)
(set-specifier default-gutter-visible-p nil)
(setq progress-feedback-use-echo-area t)

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

;;}}}

;;; Major modes
;;{{{ c-mode

(add-hook 'c-mode-hook '(lambda ()
			  (c-set-style "k&r")
			  (setq c-basic-offset 4)))
(add-hook 'c-mode-hook '(lambda ()
			  (define-key c-mode-map "\C-c\C-c" 'compile)))
(add-hook 'c-mode-hook 'turn-on-font-lock)

(setq c-cleanup-list '(brace-else-brace defun-close-semi))

;;}}}
;;{{{ makefile-mode

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (define-key makefile-mode-map "\C-c\C-c" 'compile)))

;;}}}
;;{{{ sql-mode

(add-hook 'sql-mode-hook 'turn-on-font-lock)
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)

;;}}}
;;{{{ text-mode and indented-text-mode

(require 'filladapt)

;; Enable auto-fill.
(add-hook 'text-mode-hook
	  (function (lambda ()
		      (filladapt-mode 1))))
(add-hook 'indented-text-mode-hook
	  (function (lambda ()
		      (filladapt-mode 1))))
(add-hook 'message-mode-hook
	  (function (lambda ()
		      (auto-fill-mode 1))))
(add-hook 'xml-mode-hook
	  (function (lambda ()
		      (auto-fill-mode 1))))

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
;;{{{ BBDB

(require 'bbdb)
(bbdb-initialize 'gnus 'message 'w3)

(setq bbdb-electric-p nil)

;; XXX need to generalize this to concept of "home machine"
;;(unless (string= hostname "challah")
;;  (setq bbdb-file-remote "/[challah].bbdb"))

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-default-area-code nil)

(setq bbdb-notice-auto-save-file t)

(setq bbdb-pop-up-target-lines 3)
;; On a 25-line terminal, the BBDB takes up too much space.  This
;; isn't a strictly correct fix; BBDB ought to check height at the
;; time it is thinking about whether or not to pop up a window, and
;; bbdb-pop-up-target-lines should probably just be a lambda function
;; that could return a percentage of (frame-height) or something
;; similar.  But this fixes the immediate problem.
(when (< (frame-height) 40)
  (setq bbdb-use-pop-up nil))

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist
      '(("Organization" (".*" company "\\&"))
	("Organisation" (".*" company "\\&"))
	("X-Organization" (".*" company "\\&"))
	("X-Organisation" (".*" company "\\&"))))

(setq bbdb-send-mail-style 'message)

(add-hook 'bbdb-list-hook 'close-bbdb-if-no-entries)
(defun close-bbdb-if-no-entries ()
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (if (not bbdb-records)
	(delete-window (get-buffer-window bbdb-buffer-name)))))

(add-hook 'bbdb-list-hook
	  (function (lambda ()
		      (set-specifier horizontal-scrollbar-visible-p nil))))

(setq bbdb-completion-type 'primary-or-name)

(setq bbdb-hashtable-size 100003)

;; needs my patch
(require 'bbdb-pgp)
(setq bbdb/pgp-method 'mml-pgpmime)

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

;; For calculating the sunrise and sunset times.
(setq calendar-latitude 38.909177)
(setq calendar-longitude -77.044371)
(setq calendar-location-name "Dupont Circle")

;; Use local (US Eastern) time, not my usual TZ, which is UTC.
(setq calendar-time-zone -300)
(setq calendar-standard-time-zone-name "EST")
(setq calendar-daylight-time-zone-name "EDT")
(setq calendar-daylight-savings-starts
      '(calendar-nth-named-day 1 0 4 year))
(setq calendar-daylight-savings-ends
      '(calendar-nth-named-day -1 0 10 year))
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

;;}}}
;;{{{ jka-compr

(jka-compr-install)

;;}}}
;;{{{ Mailcrypt

(load-library "mailcrypt")

(mc-setversion "gpg")

(setq mc-passwd-timeout 900)

(setq mc-gpg-fetch-keyring-list
      '("/usr/share/keyrings/debian-keyring.gpg"))
(setq mc-pgp-fetch-keyring-list
      '("/usr/share/keyrings/debian-keyring.pgp"))

(setq mc-gpg-user-id "7F9F872C")
;; should be in .gnus.el?:
(setq pgg-default-user-id mc-gpg-user-id)
(setq pgg-passphrase-cache-expiry 1200)

;;}}}
;;{{{ Perl modes

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

;;(cond ((or (string-equal (downcase (system-name)) "challah.msrl.com")
;;	   (string-equal (downcase (system-name)) "apple-pie.msrl.com"))
;;       (setq url-proxy-services '(("http" . "localhost:3128")
;;				  ("ftp" . "localhost:3128")))))

(setq url-keep-history nil)

(setq browse-url-browser-function 'browse-url-mozilla)

;;}}}
