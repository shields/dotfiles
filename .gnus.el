;;; .gnus.el --- Shields's Gnus initialization file

;; Author: Michael Shields <shields@msrl.com>
;; Version: $Id$

(setq mail-from-style 'parens)
(setq mail-yank-prefix "> ")
(add-hook 'mail-mode-hook 'turn-on-filladapt-mode)
(setq mail-header-separator "")
(setq sendmail-program "/usr/sbin/sendmail")

(setq gnus-interactive-exit nil)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq gnus-select-method
      '(nnimap "msrl"
	       (nnimap-address "127.0.0.1")
	       (nnimap-server-port 1430)))
(setq gnus-secondary-select-methods
      '((nntp "news.cis.dfn.de")))

;; Be sure outbound mail is secured also, through an ssh tunnel.
(setq message-send-mail-function 'smtpmail-send-it)
(setq gnus-agent-send-mail-function 'smtpmail-send-it)
(setq smtpmail-default-smtp-server "127.0.0.1")
(setq smtpmail-smtp-service 2500)
 
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topics)
(setq gnus-subscribe-options-newsgroup-method 'gnus-subscribe-topics)

(setq gnus-kill-files-directory "~/News/Score/")

;; All groups in the primary select method are total-expirable.
(setq gnus-total-expirable-newsgroups "^[^:]*$")

(setq gnus-default-adaptive-score-alist
      '((gnus-kill-file-mark)
	(gnus-unread-mark)
	(gnus-ticked-mark (from 10) (subject 50))
	(gnus-read-mark (from 3) (subject 30))
	(gnus-catchup-mark (subject -10))
	(gnus-killed-mark (from -1) (subject -20))
	(gnus-del-mark (from -2) (subject -15))))
(setq gnus-decay-scores t)

(require 'gnus-art)  ; hack to get value of gnus-ignored-headers
(setq gnus-visible-headers nil)
(add-to-list 'gnus-ignored-headers "^Mail-Copies-To:")
(add-to-list 'gnus-ignored-headers "^Thread-") ; some Outlook thing
(add-to-list 'gnus-ignored-headers "^User-Agent:")
(add-to-list 'gnus-ignored-headers "^X-Apparently-To: .*@yahoogroups\.com")
(add-to-list 'gnus-ignored-headers "^X-Bogosity: No,")
(add-to-list 'gnus-ignored-headers "^X-eGroups-")
(add-to-list 'gnus-ignored-headers "^X-Habeas-")
(add-to-list 'gnus-ignored-headers "^X-LYRIS-Message-Id:")
(add-to-list 'gnus-ignored-headers "^X-Mailer:")
(add-to-list 'gnus-ignored-headers "^X-Newsreader:")
(add-to-list 'gnus-ignored-headers "^X-Spam-Checker-Version:")
(add-to-list 'gnus-ignored-headers "^X-Spam-Status: No, hits=-")
(add-to-list 'gnus-ignored-headers "^X-Spam-Status: No, hits=[0-2]\\.")
(add-to-list 'gnus-ignored-headers "^X-Yahoo-Profile:")
;; for my scrapers:
(add-to-list 'gnus-ignored-headers "^X-Followup-URL:")
(add-to-list 'gnus-ignored-headers "^X-Userpic-URL:") ; need to make this appear

(setq message-ignored-news-headers
      "^NNTP-Posting-Host:\\|^Xref:\\|^[BGF]cc:\\|^Resent-Fcc:\\|^X-Draft-From:")
(setq message-ignored-mail-headers
      "^[GF]cc:\\|^Resent-Fcc:\\|^Xref:\\|^X-Draft-From:")

(add-hook 'gnus-group-mode-hook
	  (lambda ()
	    (define-key gnus-group-mode-map [j] 'next-line)
	    (define-key gnus-group-mode-map [k] 'previous-line)
	    (define-key gnus-group-mode-map [(control j)] 'gnus-group-jump-to-group)
	    (define-key gnus-group-mode-map [(home)] '(lambda ()
							(interactive)
							(gnus-summary-jump-to-group
							 gnus-inbox-name)))))
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (define-key gnus-summary-mode-map [b] 'gnus-summary-prev-page)
	    (define-key gnus-summary-mode-map [(return)] 'gnus-summary-next-unread-article)
	    (define-key gnus-summary-mode-map [(tab)] 'scroll-up-command)
	    (define-key gnus-summary-mode-map [(shift tab)] 'scroll-up-command)
	    (define-key gnus-summary-mode-map [j] 'gnus-summary-next-thread)
	    (define-key gnus-summary-mode-map [k] 'gnus-summary-prev-thread)
	    (define-key gnus-summary-mode-map [(control j)] 'gnus-summary-goto-article)
	    (define-key gnus-summary-mode-map [(control k)] 'gnus-summary-kill-thread)
	    (define-key gnus-summary-mode-map [(meta n)] 'gnus-summary-next-thread)
	    (define-key gnus-summary-mode-map [(meta p)] 'gnus-summary-prev-thread)
	    (define-key gnus-summary-mode-map [(F)]
	      'summary-followup-with-original-super-citation)))
(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (define-key gnus-article-mode-map [b] 'gnus-summary-prev-page)
	    (define-key gnus-article-mode-map [(return)] 'gnus-summary-next-unread-article)
	    (define-key gnus-article-mode-map [(F)]
	      'summary-followup-with-original-super-citation)))

(load "gnus-ml")
(add-hook 'gnus-summary-mode-hook (lambda () (gnus-mailing-list-mode 1)))

(setq gnus-default-article-saver 'gnus-summary-save-in-mail)

(setq bbdb/news-auto-create-p
      '(lambda ()
	 (not (not (or (string-equal "INBOX" gnus-newsgroup-name)
		       (string-equal "INBOX.Ebay" gnus-newsgroup-name)
		       (string-equal "INBOX.big-internet" gnus-newsgroup-name)
		       (string-equal "INBOX.bugtraq" gnus-newsgroup-name)
		       (string-equal "INBOX.cryptography" gnus-newsgroup-name)
		       (string-equal "INBOX.cypherpunks" gnus-newsgroup-name)
		       (string-equal "INBOX.end2end" gnus-newsgroup-name)
		       (string-equal "INBOX.fork" gnus-newsgroup-name)
		       (string-equal "INBOX.forum" gnus-newsgroup-name)
		       (string-equal "INBOX.fsb" gnus-newsgroup-name)
		       (string-equal "INBOX.leapsecs" gnus-newsgroup-name)
		       (string-equal "INBOX.linux-elitists" gnus-newsgroup-name)
		       (string-equal "INBOX.nanog" gnus-newsgroup-name)
		       (string-equal "INBOX.pennsic" gnus-newsgroup-name)
		       (string-match "INBOX\\.risks-" gnus-newsgroup-name)
		       (string-equal "INBOX.tz" gnus-newsgroup-name)
		       (string-equal "INBOX.xabov" gnus-newsgroup-name)
		       (string-match "INBOX\\.ietf\\." gnus-newsgroup-name)
		       (string-match "^\\([^:]*:\\)?comp\\.arch"
				     gnus-newsgroup-name)
		       (string-match "^\\([^:]*:\\)?comp\\.lang\\.perl\\.moderated"
				     gnus-newsgroup-name)
		       (string-match "^\\([^:]*:\\)?comp\\.std\\.c"
				     gnus-newsgroup-name)
		       (string-match "^\\([^:]*:\\)?comp\\.society\\.privacy"
				     gnus-newsgroup-name))))))

(setq gnus-posting-styles
      '((".*"
	 (name "Michael Shields")
	 (address "shields@msrl.com")
	 (signature "Shields.")
	 (organization "Mad Science Research Labs"))
	((message-news-p)
	 ("Mail-Copies-To" "never"))
        ("^INBOX.Ebay"
	 (address "seb@msrl.com"))))

(setq gnus-message-archive-method
      '(nnfolder "archive"
		 (nnfolder-directory   "~/Mail/archive")
		 (nnfolder-active-file "~/Mail/archive/active")
		 (nnfolder-get-new-mail nil)
		 (nnfolder-inhibit-expiry t)))
(setq gnus-message-archive-group
      '((cond ((not gnus-newsgroup-name)
	       nil)
	      ((string-match "^\\(nnfolder\\+archive:\\)?mfnx\\."
			     gnus-newsgroup-name)
	       (format-time-string "mfnx.OUT.%Y-%m"))
	      ((string-equal "INBOX.Ebay" gnus-newsgroup-name)
	       (format-time-string "ebay.OUT.%Y-%m")))))

(setq gnus-gcc-mark-as-read t)

(setq gnus-large-newsgroup 2000)

(setq gnus-keep-same-level 'best)

;;(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
;;(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)

(setq gnus-keep-backlog 20)

(setq gnus-prompt-before-saving t)

(setq gnus-cite-attribution-face 'default)  ; not underline
(setq gnus-treat-display-picons nil)   ; includes x-face
(setq gnus-treat-display-smileys nil)
(setq gnus-treat-date-ut nil)
(setq gnus-treat-date-user-defined '(or head (typep "message/rfc822")))
(add-hook 'gnus-part-display-hook 'gnus-article-date-user)
(setq gnus-treat-emphasize nil)
(setq gnus-article-time-format "%Y-%m-%d %H:%M:%S %Z")
(add-hook 'gnus-part-display-hook 'gnus-article-date-user)
(setq gnus-treat-hide-boring-headers 'head)
(setq gnus-boring-article-headers
      '(empty followup-to reply-to newsgroups to-address))

;; this is used for gnus-summary-*-ancient-face via custom:
(make-face 'face-for-ancient-articles)
(set-face-foreground 'face-for-ancient-articles "midnightblue")
(make-face 'face-for-ancient-articles-bold)
(set-face-foreground 'face-for-ancient-articles-bold "midnightblue")
;;(make-face-bold 'face-for-ancient-articles-bold)

(setq gnus-cite-max-parse-size (* 100 1024))  ; up from 25000

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq message-citation-line-function nil)
(defun insert-super-citation-line ()
  (let* ((was-modified (buffer-modified-p))
	 (message-id (mail-header-id message-reply-headers))
	 (first-line
	  (cond ((string-match "\\bINBOX\\.risks\\b" gnus-newsgroup-name)
		 "In RISKS Digest,")
		((string-match "^<LYRIS-.*@.*>$" message-id)
		 nil)
		((nnheader-fake-message-id-p message-id)
		 nil)
		(t
		 (concat (if (message-news-p) "In article " "In message ")
			 (mail-header-id message-reply-headers) ","))))
	 (address
	  (car (cdr (mail-extract-address-components
		     (mail-header-from message-reply-headers)))))
	 (second-line
	  (concat (if (string-equal address user-mail-address)
		      "I"
		    (mail-header-from message-reply-headers)) " wrote:\n")))
    (when first-line
      (insert first-line
	      (if (> (+ (length first-line) (length second-line)) fill-column)
		  "\n" " ")))
    (insert second-line)
    (set-buffer-modified-p was-modified)))
(defun summary-followup-with-original-super-citation (n &optional force-news)
  "Replacement for `gnus-summary-followup-with-original' to use super
citation lines."
  (interactive "P")
  (let ((message-citation-line-function 'insert-super-citation-line))
    (gnus-summary-followup (gnus-summary-work-articles n) force-news)))

;; Don't quote on `+' or `}'; those mess up patches and code especially.
(setq gnus-cite-prefix-regexp "^[]>|: ]*[]>|:]\\(.*>\\)?\\|^.*>")

(setq gnus-save-newsrc-file nil)

(setq gnus-use-cache t)
(setq gnus-uncacheable-groups "^[^:]*$")

(setq gnus-score-find-score-files-function
      '(gnus-score-find-bnews bbdb/gnus-score))

(setq gnus-optional-headers 'bbdb/gnus-lines-and-from)

;; This is the same as the stock version, except with a bbdb mark-char
;; added, and with BBDB names replacing standard %n names.
(setq gnus-summary-line-format
      "%U%R%z%ub%I%(%[%4L:%-23,23uB%]%) %s\n")
(setq gnus-summary-zcore-fuzz 4)
(setq bbdb/gnus-header-prefer-real-names t)
;; Not the same as (setq bbdb/gnus-mark-known-posters nil):
(setq bbdb/gnus-summary-known-poster-mark " ")
;; This affects only %uB; we get the info onscreen with %ub earlier in
;; the gnus-summary-line-format:
(setq bbdb/gnus-summary-mark-known-posters nil)

(require 'mailcrypt)
(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
(add-hook 'message-mode-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

;; This ugly hack is the the Gnus manual, so I guess there is no
;; better way to do this yet.
(eval-after-load "w3"
  '(progn
     (fset 'w3-fetch-orig (symbol-function 'w3-fetch))
     (defun w3-fetch (&optional url target)
       (interactive (list (w3-read-url-with-default)))
       (if (eq major-mode 'gnus-article-mode)
	   (browse-url url)
	 (w3-fetch-orig url target)))))

(setq gnus-button-url 'gnus-netscape-open-url)
(setq browse-url-new-window-flag t)

(setq message-dont-reply-to-names "\\(m?shields@\\(msrl\\.com\\|mfnx\\.net\\|above\\.net\\|iad\\.above\\.net\\)\\|michael\\.shields@m?mfn\\.com\\|seb@msrl\.com\\)")

(setq gnus-signature-limit 30.0)

;; Faster than doing this with elisp.
(setq base64-encoder-program "/usr/local/bin/base64")
(setq base64-encoder-switches '("-e"))
(setq base64-decoder-program "/usr/local/bin/base64")
(setq base64-decoder-switches '("-d"))

(setq message-forward-before-signature nil)

;; From the manual; don't autosave nnfolder files:
(defun turn-off-backup ()
  (set (make-local-variable 'backup-inhibited) t))
(add-hook 'nnfolder-save-buffer-hook 'turn-off-backup)

;; Allow for multiple open windows on different groups (although, BBDB
;; still displays only in one buffer; oh well).
(setq gnus-single-article-buffer nil)

(require 'gnus-delay)
(gnus-delay-initialize)

(gnus-demon-add-handler 'gnus-group-get-new-news 5 15)

;; Don't move forward in group buffer with M-g.
(setq gnus-goto-next-group-when-activating nil)

;; Don't jump over articles after setting a mark.  (It is common to
;; want to remark several ticked articles, for example.)
(setq gnus-summary-goto-unread nil)

(setq mm-coding-system-priorities '(us-ascii iso-latin-1 utf-8))

(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)
;; Only for classic PGP, not PGP/MIME:
(add-hook 'gnus-article-hide-pgp-hook
	  (lambda ()
	    (save-excursion
	      (set-buffer gnus-original-article-buffer)
	      (mc-verify))))
;; Display visible confirmation that the mail was encrypted:
(setq gnus-buttonized-mime-types
      (append (list "multipart/signed" "multipart/encrypted")
	      gnus-buttonized-mime-types))

;; Update marks in real time, so that the mail checker widget can have
;; current information.  This should really be on gnus-summary-mark-article,
;; but there is no appropriate hook for that.
(add-hook 'gnus-select-article-hook
	  (lambda ()
	    (when (string= gnus-newsgroup-name "INBOX")
	      (gnus-summary-update-info t))))


(require 'spam)
(setq spam-junk-mailgroups '("INBOX.Spam"))
(setq gnus-spam-process-newsgroups
      '(("^INBOX" (gnus-group-ham-exit-processor-copy))))
(setq gnus-spam-process-destinations
      '(("^INBOX" "INBOX.SA-spam")))
(setq gnus-ham-process-destinations
      '(("^INBOX" "INBOX.SA-ham")))
(setq spam-move-spam-nonspam-groups-only nil)


;; This is slow.  Maybe because a keystroke doesn't get processed
;; until the article being prefetched is down downloading?  Should
;; look into it.  XXX
;;(setq gnus-asynchronous t)


(setq gnus-article-banner-alist
      '((debian
	 . "^-- \nTo UNSUBSCRIBE,.*\n.*")
	(mailman
	 . "^_________+\n.* mailing list\n.*\\(@.*\n\\)?http.*\n")
	(sourceforge
	 . "^----------+\nThis \\(SF\\.NET\\|sf\\.net\\) email is sponsored by:\\(.*\n\\)+")
	(yahoogroups
	 . "^------+ Yahoo! Groups Sponsor ------+~-->\n\\(.*\n\\)+")))

(setq gnus-article-address-banner-alist
      '(("@address\\.com$" . "^---------+\nGet FREE INTERNET .*\n.*")
	("@flashmail\\.com$" . "^_________+\n.*\n.*")
	("@hotmail\\.com$" . "^_________+\n.*\n.*")
	("@netscape\\.net$" . "^_________+\n.*\n.*\nGet your own FREE, personal Netscape Mail account.*")
	("@juno\\.com$" . "^_________+\n.*\n.*")
	("@yahoo\\.com$" . "^\\(----------+\\|__________+\\)\nDo you Yahoo!\\?\n.*\n.*")
	("." . "^-----= Posted via Newsfeeds\.Com, .* =-----\n.*\n-----=.*=-----\n")
	("." . "^---\nOutgoing mail is certified Virus Free\\.\nChecked by AVG .*\nVersion: .*")))


(setq gnus-message-archive-group nil)
(setq gnus-outgoing-message-group "INBOX.Outbox")

(setq nnmail-expiry-wait 14)

(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)

;; from the manual:
(setq gnus-refer-article-method
      '(current
	(nnweb "refer" (nnweb-type google))))

(add-hook 'message-sent-hook 'gnus-score-followup-thread)

(setq gnus-prefetched-article-deletion-strategy '(exit))

(setq gnus-group-line-format
      "%M%S%p%P%10uu: %(%ug%)%ut\n")
(defun gnus-user-format-function-g (dummy)
  "Return the name of the group, with \"INBOX.\" trimmed off the front."
  (if (string-match "^INBOX\\." gnus-tmp-qualified-group)
      (substring gnus-tmp-qualified-group 6)
    gnus-tmp-qualified-group))
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
(defvar gnus-group-notably-old 12
  "How many hours before a group will be flagged as not recently read
in the group buffer.")
(defun gnus-user-format-function-t (dummy)
  "Return a string if the group is older than `gnus-group-notably-old'."
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (if (null time)
	"  (never)"
      (let ((age (time-to-seconds (time-subtract (current-time) time))))
	(if (< age (* 3600 gnus-group-notably-old))
	    ""
	  (format "  (%.1f)" (/ age 86400)))))))
(defun gnus-user-format-function-u (dummy)
  "Return a string indicating how many articles are unread/unseen."
  (let* ((unseen (gnus-number-of-unseen-articles-in-group gnus-tmp-group))
	 (seen-but-unread
	  (- (string-to-number gnus-tmp-number-of-unread) unseen)))
    (if (= seen-but-unread 0)
	(number-to-string unseen)
      (format "%d + %d" seen-but-unread unseen))))
(gnus-compile)


(setq gnus-article-skip-boring t)


(add-hook 'gnus-part-display-hook 'maybe-correct-for-outlook)
(defun maybe-correct-for-outlook ()
  "Look for Microsoft Outlook headers, and try to fix some of the problems."
  (let ((ua (or (gnus-fetch-field "X-Mailer")
		(gnus-fetch-field "X-Newsreader"))))
    (when (and ua
	       (string-match "^Microsoft Outlook[, ]" ua))
      (gnus-article-treat-dumbquotes t)
      (gnus-article-outlook-unwrap-lines t))))

(add-hook 'gnus-part-display-hook 'maybe-correct-for-yahoogroups)
(defun maybe-correct-for-yahoogroups ()
  "Look for Yahoo Groups headers, and try to fix some of the problems."
  (when (string= (gnus-fetch-field "X-Mailer")
		 "Yahoo Groups Message Poster")
    (gnus-article-outlook-unwrap-lines t)))


(setq mail-extr-ignore-single-names nil)


(add-hook 'gnus-select-group-hook 'gnus-group-get-new-news-this-group)
(add-hook 'gnus-select-group-hook 'gnus-summary-insert-new-articles)


(add-to-list 'nnmail-extra-headers 'X-Spam-Status)
(defun gnus-article-sort-by-spam-status (h1 h2)
  "Sort articles by score from the X-Spam-Status: header."
  (< (string-to-number (gnus-replace-in-string
			(gnus-extra-header 'X-Spam-Status h1)
			".*hits=" ""))
     (string-to-number (gnus-replace-in-string
			(gnus-extra-header 'X-Spam-Status h2)
			".*hits=" ""))))


(eval-after-load "w3m"
  '(progn
     (defun override-w3m-view-this-url ()
       "View the URL of the link under point, but using `browse-url'."
       (interactive)
       (let ((url (w3m-anchor)))
	 (cond
	  (url (browse-url url))
	  (t (w3m-safe-view-this-url)))))
     (define-key w3m-minor-mode-map [(return)] 'override-w3m-view-this-url)))



(defvar gnus-topic-needs-mumbling nil)
(defun gnus-topic-mumble ()
  (when gnus-topic-needs-mumbling
    (setq gnus-topic-needs-mumbling nil)
    (gnus-group-sort-groups-by-mumble t)))
;;  (gnus-group-next-unread-group))
(add-hook 'gnus-group-prepare-hook 'gnus-topic-mumble)
(add-hook 'gnus-summary-exit-hook
	  (lambda ()
	    (setq gnus-topic-needs-mumbling t)))
;; Much here adapted from gnus-group-sort-by-unread.
(defun gnus-group-sort-by-mumble (info1 info2)
  "Sort by weighted blah blah.
Number of unread articles plus 10 times hours since last read
(never = 30 days)."
  (let* ((g1 (gnus-info-group info1))
	 (g2 (gnus-info-group info2))
	 (n1 (car (gnus-gethash g1 gnus-newsrc-hashtb)))
	 (n2 (car (gnus-gethash g2 gnus-newsrc-hashtb)))
	 (lr1 (gnus-group-timestamp g1))
	 (lr2 (gnus-group-timestamp g2))
	 (now (current-time))
	 (a1 (if lr1
		 (/ (time-to-seconds (time-subtract now lr1)) 3600)
	       (* 30 24)))
	 (a2 (if lr2
		 (/ (time-to-seconds (time-subtract now lr2)) 3600)
	       (* 30 24))))
    (< (+ (* 10 a1) (or (and (numberp n1) n1) 0))
       (+ (* 10 a2) (or (and (numberp n2) n2) 0)))))
(defun gnus-group-sort-groups-by-mumble (&optional reverse)
  "Sort the group buffer by mumble.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-group-sort-groups 'gnus-group-sort-by-mumble reverse))
(defun gnus-group-sort-selected-groups-by-mumble (&optional n reverse)
  "Sort the group buffer by mumble.
Obeys the process/prefix convention.  If REVERSE (the symbolic prefix),
sort in reverse order."
  (interactive (gnus-interactive "P\ny"))
  (gnus-group-sort-selected-groups n 'gnus-group-sort-by-mumble reverse))
(defun gnus-topic-sort-groups-by-mumble (&optional reverse)
  "Sort the current topic by mumble.
If REVERSE, sort in reverse order."
  (interactive "P")
  (gnus-topic-sort-groups 'gnus-group-sort-by-mumble reverse))



(setq gnus-keep-same-level t)



(defun safe-gnus-summary-catchup-and-goto-next-group ()
  "Mark all articles in this group as read and select the next group.

This function is exactly like `gnus-summary-catchup-and-goto-next-group',
but it doesn't accept a prefix argument (which can wipe out all articles)."
  (interactive)
  (save-excursion
    (gnus-summary-catchup))
  (gnus-summary-next-group))
(define-key gnus-summary-mode-map
  "c" 'safe-gnus-summary-catchup-and-goto-next-group)
