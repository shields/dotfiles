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
(setq gnus-secondary-select-methods nil)

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

(setq gnus-visible-headers nil)
(setq gnus-ignored-headers
      "^Xref:\\|NNTP-Posting-\\|^X-Trace:\\|^X-Complaints-To:\\|^Lines:\\|^X-From-Line:\\|^Path:\\|^X-Newsreader:\\|^X-Nntp-Posting-\\|^X-No-Archive:\\|^X-BOFH-Archive:\\|^Mail-Copies-To:\\|^Resent-\\|^X-Mailing-List:\\|^X-Loop:\\|^Precedence:\\|^Approved:\\|^X-Original-Date:\\|^Originator:\\|^From \\|^Return-Path:\\|^Received:\\|^In-Reply-To:\\|^Message-Id:\\|^Sender:\\|^X-Mailer:\\|^MIME-\\|^Content-\\|^X-VM-\\|^X-Sender:\\|^References:\\|^Precedence:[ \t]+bulk\\|^X-Face\\|^Delivered-To:\\|^Mailing-List:\\|^Status:\\|^X-Listprocessor-Version:\\|^X-Authentication-Warning:[^:]*: majordom set\\|^Lines:\\|^Mail-Copies-To:\\|^X400-\\|^X-Priority:\\|^X-MSMail-Priority:\\|^X-Content-Length:\\|^X-Orcpt:\\|^X-MimeOLE:\\|^Illegal-Object:\\|^X-UIDL:\\|^X-MIME-Autoconverted:\\|^Approved-By:\\|^X-VM-\\|^X-Gnus-Mail-Source:\\|^User-Agent:\\|^X-Mailinglist:\\|^List-\\(Help\\|Unsubscribe\\|Post\\|Subscribe\\):\\|^Importance:\\|^X-Exmh-\\|^X-Accept-Language:\\|^X-eGroups-\\|^List-Archive:\\|^Phone:\\|^Fax:\\|^Errors-To:\\|^X-BeenThere:\\|^X-Mailman-Version:\\|^List-Id:\\|^X-Authentication-Warning: [^:]*: majordomo \\|^X-LYRIS-Message-Id:\\|^Cancel-Lock:\\|^X-Spam-Status:\\|^X-Yahoo-Profile:\\|^X-AntiAbuse:\\|^X-Habeas-")
(setq message-ignored-news-headers
      "^NNTP-Posting-Host:\\|^Xref:\\|^[BGF]cc:\\|^Resent-Fcc:\\|^X-Draft-From:")
(setq message-ignored-mail-headers
      "^[GF]cc:\\|^Resent-Fcc:\\|^Xref:\\|^X-Draft-From:")

(add-hook 'gnus-group-mode-hook
	  (lambda ()
	    (define-key gnus-group-mode-map [j] 'next-line)
	    (define-key gnus-group-mode-map [k] 'previous-line)
	    (define-key gnus-group-mode-map [(control j)] 'gnus-group-jump-to-group)))
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (define-key gnus-summary-mode-map [b] 'gnus-summary-prev-page)
	    (define-key gnus-summary-mode-map [(return)] 'gnus-summary-next-unread-article)
	    (define-key gnus-summary-mode-map [(tab)] 'scroll-up-command)
	    (define-key gnus-summary-mode-map [j] 'next-line)
	    (define-key gnus-summary-mode-map [k] 'previous-line)
	    (define-key gnus-summary-mode-map [(control j)] 'gnus-summary-goto-article)
	    (define-key gnus-summary-mode-map [(control k)] 'gnus-summary-kill-thread)))
(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (define-key gnus-article-mode-map [b] 'gnus-summary-prev-page)
	    (define-key gnus-article-mode-map [(return)] 'gnus-summary-next-unread-article)))

(load "gnus-ml")
(add-hook 'gnus-summary-mode-hook (lambda () (gnus-mailing-list-mode 1)))

(setq gnus-default-article-saver 'gnus-summary-save-in-mail)

(remove-hook 'gnus-select-group-hook 'turn-gnus-bbdb-on-or-off)
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
		       (string-equal "INBOX.fsb" gnus-newsgroup-name)
		       (string-equal "INBOX.leapsecs" gnus-newsgroup-name)
		       (string-equal "INBOX.nanog" gnus-newsgroup-name)
		       (string-equal "INBOX.pennsic" gnus-newsgroup-name)
		       (string-match "INBOX\\.risks-" gnus-newsgroup-name)
		       (string-equal "INBOX.tz" gnus-newsgroup-name)
		       (string-equal "INBOX.xabov" gnus-newsgroup-name)
		       (string-match "INBOX\\.ietf\\." gnus-newsgroup-name))))))

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
(setq gnus-treat-date-ut t)
(setq gnus-treat-emphasize nil)
(setq gnus-article-time-format "%Y-%m-%d %H:%M:%S %Z")
(add-hook 'gnus-part-display-hook 'gnus-article-date-user)

;; this is used for gnus-summary-*-ancient-face via custom:
(make-face 'face-for-ancient-articles)
(set-face-foreground 'face-for-ancient-articles "midnightblue")
(make-face 'face-for-ancient-articles-bold)
(set-face-foreground 'face-for-ancient-articles-bold "midnightblue")
;;(make-face-bold 'face-for-ancient-articles-bold)

(setq gnus-cite-max-parse-size (* 100 1024))  ; up from 25000

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq message-citation-line-function 'insert-trn-style-citation-line-plus)
(defun insert-trn-style-citation-line-plus ()
    (when (and message-reply-headers
	       (not (string-equal gnus-newsgroup-name "INBOX"))
	       (not (string-equal gnus-newsgroup-name "INBOX.Ebay")))
      (let* ((first-line
	      (if (string-equal gnus-newsgroup-name "INBOX.risks")
		  "In RISKS Digest,"
		(concat "In article " (mail-header-id message-reply-headers) ",")))
	     (his-address
	      (car (cdr (mail-extract-address-components
			 (mail-header-from message-reply-headers)))))
	    (second-line
	     (concat (if (string-equal his-address user-mail-address)
			 "I"
		       (mail-header-from message-reply-headers)) " wrote:\n")))
	(cond ((string-match "^INBOX.rennlist-" gnus-newsgroup-name)
	       (insert second-line))
	      ((not (string-equal his-address "tickets@tickets.above.net"))
	       (insert first-line
		       (if (> (+ (length first-line) (length second-line)) fill-column)
			   "\n" " ")
		       second-line))))))

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

(setq message-dont-reply-to-names "\\(m?shields@\\(msrl\\.com\\|mfnx\\.net\\|above\\.net\\|iad\\.above\\.net\\)\\|michael\\.shields@m?mfn\\.com\\|seb@msrl\.com\\)")

;; Controls C-x m in message mode
(setq mail-user-agent 'message-user-agent)

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
