;;; .emacs --- Shields's Emacs initialization file

;; Author: Michael Shields <shields@msrl.com>
;; Version: 2001-08-18

;;; Globals
;;{{{ Preliminaries

;; Must elegantize this:
(setq load-path (cons (expand-file-name "~/share/xemacs/site-lisp/gnus")
		      load-path))

;; /etc/xemacs21/site-start.d/20gnus-init.el instantiates the system
;; version of Gnus.  Be sure to override it.
(provide 'gnus-init)   ; XXX will this work to suppress gnus-init?
(load "gnus")

(require 'info)
(setq Info-directory-list (cons "~/info" Info-default-directory-list))

;; Enable correct handling of ISO8859-1, and the C-x 8 compose key.
;; *Note (emacs)European Display::.
(standard-display-european 1)
(set-input-mode (car (current-input-mode)) ; High bit is not meta
		(nth 1 (current-input-mode))
		0)
(setq hexl-iso "-iso")
(require 'x-compose)
(global-set-key "\C-x8" compose-map)

(setq user-mail-address "shields@msrl.com")

;; Protect .saves-PID-HOST file.  Adapted from Mike Long <mikel@shore.net>.
(and (boundp 'auto-save-list-file-name)
     (not (null auto-save-list-file-name)) ; added by Tim Moore <tmoore@tembel.org>
     (progn
       (or (file-exists-p auto-save-list-file-name)
	   (write-region "" nil auto-save-list-file-name nil 'quiet))
       (set-file-modes auto-save-list-file-name 384)))

(autoload 'debian-changelog-mode "debian-changelog-mode" nil t)

;; umask
(set-default-file-modes 448)  ; 0700

;;}}}
;;{{{ Customization of commands

;; Disabled commands.  Hmmph.
(put 'eval-expression 'disabled nil)	; C-:
(put 'narrow-to-region 'disabled nil)	; C-x n n
;;(setq disabled-command-hook 'ignore)

;; Stop C-n from adding newlines.  C-f doesn't add anything.  C-p
;; doesn't.  It's weird.
(setq next-line-add-newlines nil)

;;}}}
;;{{{ Display

(setq inhibit-startup-message t)

(setq search-slow-speed 2400)
(setq search-slow-window-lines 3)

;;(setq visible-bell t)

;; Don't close windows because they're too short.  Sometimes a
;; single-line (plus mode line) window can be useful.
(setq window-min-height 2)

(setq display-time-24hr-format t)
;; When it wakes up from an APM sleep, runs a timer for each minute!
;;(display-time)
(setq display-time-mail-file "~/var/maildirs/crosslink.net/biff")

(set-face-background 'default "white")
(set-face-background 'modeline "grey88")

(setq blink-matching-delay 0.25)

;;XXX(menu-bar-mode -1)
(set-specifier menubar-visible-p nil)  ;;XXX
(set-specifier default-toolbar-visible-p nil)  ;XXX

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
(global-set-key "\C-m" 'newline-and-indent)

;; C-x M-, will search; M-, will repeat.
(global-set-key "\C-x\M-," 'tags-search)
      
;; Like folding sets it up, and convenient.
(global-set-key "\M-g" 'goto-line)

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
;;{{{ html-mode

;;(autoload 'html-mode "html-mode" "HTML major mode." t)

;;(or (assoc "\\.html$" auto-mode-alist)
;;    (setq auto-mode-alist (cons '("\\.html$" . html-mode)
;;				auto-mode-alist)))

;;(add-hook 'html-mode-hook
;;	  (function (lambda ()
;;		      (auto-fill-mode 1))))

;;}}}
;;{{{ makefile-mode

(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (define-key makefile-mode-map "\C-c\C-c" 'compile)))

;;}}}
;;{{{ perl-mode

;; Until perl-mode has intelligence about filling, just use filladapt,
;; which is smart enough about the comments.
(add-hook 'perl-mode-hook 'turn-on-filladapt-mode)

;;}}}
;;{{{ sgml-mode

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)

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

;; Perl extension glues.  Not really like C; more like a Makefile.
(or (assoc "\\.xs$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.xs$" . indented-text-mode)
				auto-mode-alist)))

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

;;; Features
;;{{{ Ange-ftp / EFS

;;XXX(require 'ange-ftp)

;; Use anonymous by default, under the assumption that other machines
;; will be listed in .netrc.  
(setq ange-ftp-default-user "anonymous")
(setq ange-ftp-generate-anonymous-password (or (getenv "MAILTO")
					       t))

(setq efs-use-passive-mode t)

;;}}}
;;{{{ BBDB

(require 'bbdb)
(require 'gnus)
(require 'message)
(require 'w3)
(bbdb-initialize 'gnus 'message 'w3)

(setq bbdb-electric-p nil)

(setq bbdb-north-american-phone-numbers-p nil)
(setq bbdb-default-area-code nil)

(setq bbdb-notice-auto-save-file t)

(setq bbdb-pop-up-target-lines 3)

(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist
      '(("Organization" (".*" company "\\&"))
	("Organisation" (".*" company "\\&"))
	("X-Organization" (".*" company "\\&"))
	("X-Organisation" (".*" company "\\&"))
	;;  Straight from the manual for 1.5.1:
	("X-Face"
	 ("[ \t\n]*\\([^ \t\n]*\\)\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?\\([ \t\n]+\\([^ \t\n]+\\)\\)?" face "\\1\\3\\5\\7"))
	("Newsgroups" ("^bofh\." mark-char "@"))
	("Newsgroups" ("^nnml:nanog" mark-char ":"))))

(setq bbdb-send-mail-style 'vm)

(add-hook 'bbdb-list-hook 'close-bbdb-if-no-entries)
(defun close-bbdb-if-no-entries ()
  (save-excursion
    (set-buffer bbdb-buffer-name)
    (if (not bbdb-records)
	(delete-window (get-buffer-window bbdb-buffer-name)))))

(add-hook 'bbdb-list-hook
	  (function (lambda ()
		      (set-specifier horizontal-scrollbar-visible-p nil))))

;;}}}
;;{{{ Calc

;;(setq calc-group-digits t)
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
(if (and window-system
	 (= emacs-major-version 16))
    (font-lock-make-faces t))

;;}}}
;;{{{ Mailcrypt

(load-library "mailcrypt")

(setq mc-passwd-timeout 900)

(setq mc-gpg-fetch-keyring-list
      '("/usr/share/keyrings/debian-keyring.gpg"))
(setq mc-pgp-fetch-keyring-list
      '("/usr/share/keyrings/debian-keyring.pgp"))

;;}}}
;;{{{ Perl-descr

(autoload 'describe-perl-symbol "perl-descr"
  "One-line information on a perl symbol" t)
(autoload 'switch-to-perl-doc-buffer "perl-descr"
  "One-line information on a perl symbol" t)

(add-hook 'perl-mode-hook
	  (function (lambda ()
		      (define-key perl-mode-map "\M-oq"
			'describe-perl-symbol)
		      (define-key perl-mode-map "\M-od"
			'switch-to-perl-doc-buffer))))

;;}}}
;;{{{ Suspension

;; This is simple.  *Note (emacs)Resume Arguments::.
(add-hook 'suspend-hook 'resume-suspend-hook)

;;}}}
;;{{{ Version control

(require 'vc)

;; Default is "-c".
(setq diff-switches "-u")

;;}}}
;;{{{ W3

(cond ((or (string-equal (downcase (system-name)) "challah.msrl.com")
	   (string-equal (downcase (system-name)) "apple-pie.msrl.com"))
       (setq url-proxy-services '(("http" . "localhost:3128")
				  ("ftp" . "localhost:3128")))))

(setq url-keep-history nil)

(setq browse-url-browser-function 'browse-url-netscape)
;; http://home.netscape.com/newsref/std/remote.c
(setq browse-url-netscape-program "/usr/bin/X11/netscape-remote")
(setq browse-url-netscape-arguments '("-noraise"))

;;}}}

;;{{{ Local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
(custom-set-variables
 '(bbdb-gui nil)
 '(bbdb-complete-name-allow-cycling t)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(bbdb-time-display-format "%Y-%m-%d"))
(custom-set-faces
 '(gnus-group-mail-3-face ((((class color) (background light)) (:foreground "magenta4"))))
 '(gnus-summary-low-unread-face ((t (:foreground "grey18"))))
 '(gnus-summary-low-ticked-face ((((class color) (background light)) (:foreground "firebrick"))))
 '(gnus-header-content-face ((((class color) (background light)) (:foreground "indianred4"))))
 '(message-cited-text-face ((((class color) (background light)) (:foreground "indianred4"))))
 '(gnus-signature-face ((((type x)) (:foreground "grey50"))))
 '(gnus-summary-low-ancient-face ((((class color) (background light)) (:foreground "RoyalBlue"))))
 '(gnus-summary-low-read-face ((((class color) (background light)) (:foreground "DarkGreen")))))
