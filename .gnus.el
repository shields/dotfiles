;;; .gnus.el --- Shields's Gnus initialization file

;; Author: Michael Shields <shields@msrl.com>
;; Version: 2001-08-22

;; XXX http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=82226
(load "mm-decode")
(load "messagexmas")
(load "message")

(setq mail-from-style 'parens)
(setq mail-yank-prefix "> ")
(add-hook 'mail-mode-hook 'turn-on-filladapt-mode)
(setq mail-header-separator "")
(setq sendmail-program "/usr/sbin/sendmail")

(setq gnus-interactive-exit nil)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;;(setq gnus-select-method '(nntp "news.netaxs.com"))
;;(setq gnus-select-method '(nntp ""))
;;(setq gnus-secondary-select-methods '((nnml "") (nnslashdot "")))
;;(setq gnus-secondary-select-methods '((nnml "")))
(setq gnus-select-method '(nnml ""))
;;(setq gnus-secondary-select-methods nil)
(setq gnus-secondary-select-methods '((nnslashdot "")))

(setq nnml-directory "~/Gnuspool/")
(setq mail-sources
      '((directory :path (concat nnml-directory ".incoming/")
		   :suffix "")))
(setq mail-source-delete-incoming t)

(load "~/.gnus.slashdot.el")  ; contains password
(setq nnslashdot-threshold 5)
(setq nnslashdot-group-number 20)
 
(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-topics)

(setq gnus-kill-files-directory "~/News/Score/")
(setq gnus-score-file-suffix "SCORE")

(setq gnus-home-score-file
      '(("^nnslashdot:" "slashdot.SCORE")))
(setq gnus-home-adapt-file
      '(("^nnslashdot:" "slashdot.ADAPT")))

(setq gnus-check-new-newsgroups 'ask-server)
(setq gnus-read-active-file 'some)

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
      "^Xref:\\|NNTP-Posting-\\|^X-Trace:\\|^X-Complaints-To:\\|^Lines:\\|^X-From-Line:\\|^Path:\\|^X-Newsreader:\\|^X-Nntp-Posting-\\|^X-No-Archive:\\|^X-BOFH-Archive:\\|^Mail-Copies-To:\\|^Resent-\\|^X-Mailing-List:\\|^X-Loop:\\|^Precedence:\\|^Approved:\\|^X-Original-Date:\\|^Originator:\\|^From \\|^Return-Path:\\|^Received:\\|^In-Reply-To:\\|^Message-Id:\\|^Sender:\\|^X-Mailer:\\|^MIME-\\|^Content-\\|^X-VM-\\|^X-Sender:\\|^References:\\|^Precedence:[ \t]+bulk\\|^X-Face:\\|^Delivered-To:\\|^Mailing-List:\\|^Status:\\|^X-Listprocessor-Version:\\|^X-Authentication-Warning:[^:]*: majordom set\\|^Lines:\\|^Mail-Copies-To:\\|^X400-\\|^X-Priority:\\|^X-MSMail-Priority:\\|^X-Content-Length:\\|^X-Orcpt:\\|^X-MimeOLE:\\|^Illegal-Object:\\|^X-UIDL:\\|^X-MIME-Autoconverted:\\|^Approved-By:\\|^X-VM-\\|^X-Gnus-Mail-Source:\\|^User-Agent:\\|^X-Mailinglist:\\|^List-\\(Help\\|Unsubscribe\\|Post\\|Subscribe\\):\\|^Importance:\\|^X-Exmh-\\|^X-Accept-Language:\\|^X-eGroups-\\|^List-Archive:\\|^Phone:\\|^Fax:\\|^Errors-To:\\|^X-BeenThere:\\|^X-Mailman-Version:\\|^List-Id:\\|^X-Authentication-Warning: [^:]*: majordomo ")

;; When displaying an article, go back with `b', like trn.
;; Also make RET move forward an article, not a line, to avoid `n' fatigue.
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (define-key gnus-summary-mode-map [b] 'gnus-summary-prev-page)
	    (define-key gnus-summary-mode-map [(return)] 'gnus-summary-next-unread-article)))
(add-hook 'gnus-article-mode-hook
	  (lambda ()
	    (define-key gnus-article-mode-map [b] 'gnus-summary-prev-page)
	    (define-key gnus-article-mode-map [(return)] 'gnus-summary-next-unread-article)))

(setq gnus-default-article-saver 'gnus-summary-save-in-mail)

(add-hook 'gnus-select-group-hook 'turn-gnus-bbdb-on-or-off)
(defun turn-gnus-bbdb-on-or-off ()
  (setq bbdb/news-auto-create-p
	(not (not (or (string-equal "MSRL.COM" gnus-newsgroup-name)
		      (string-equal "big-internet" gnus-newsgroup-name)
		      (string-equal "bugtraq" gnus-newsgroup-name)
		      (string-equal "cryptography" gnus-newsgroup-name)
		      (string-equal "end2end" gnus-newsgroup-name)
		      (string-equal "fsb" gnus-newsgroup-name)
		      (string-equal "leapsecs" gnus-newsgroup-name)
		      (string-equal "nanog" gnus-newsgroup-name)
		      (string-equal "risks" gnus-newsgroup-name)
		      (string-equal "tz" gnus-newsgroup-name)
		      (string-match "mfnx\\." gnus-newsgroup-name)
		      (string-match "ietf\\." gnus-newsgroup-name))))))

(setq gnus-button-url 'gnus-netscape-open-url)

(setq gnus-posting-styles
      '((".*"
	 (name "Michael Shields")
	 (address "shields@msrl.com")
	 (signature "Shields.")
	 (organization "Mad Science Research Labs"))
	((message-news-p)
	 ("Mail-Copies-To" "never"))
        ("^Ebay"
	 (address "seb@msrl.com"))
	("^\\(nnfolder\\+archive:\\)?mfnx\\."
	 (address "michael.shields@mmfn.com")
	 (organization "Metromedia Fiber Network")
	 (signature "Shields, MFN."))))

(setq gnus-message-archive-method
      '(nnfolder "archive"
		 (nnfolder-directory   "~/Mail/archive")
		 (nnfolder-active-file "~/Mail/archive/active")
		 (nnfolder-get-new-mail nil)
		 (nnfolder-inhibit-expiry t)))
(setq gnus-message-archive-group
      '((cond ((string-match "^\\(nnfolder\\+archive:\\)?mfnx\\." gnus-newsgroup-name)
	       (format-time-string "mfnx.OUT.%Y-%m"))
	      ((string-equal "Ebay" gnus-newsgroup-name)
	       (format-time-string "ebay.OUT.%Y-%m")))))

(setq gnus-inews-mark-gcc-as-read t)

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

;; ah, trn.
;;(setq gnus-use-trees t)
;;(setq gnus-selected-tree-face 'highlight)
;;(setq gnus-generate-tree-function 'gnus-generate-horizontal-tree)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq message-citation-line-function 'insert-trn-style-citation-line-plus)
(defun insert-trn-style-citation-line-plus ()
    (when (and message-reply-headers
	       (not (string-equal gnus-newsgroup-name "ABOVE.NET"))
	       (not (string-equal gnus-newsgroup-name "mfnx.aleph"))
	       (not (string-equal gnus-newsgroup-name "MSRL.COM")))
      (let* ((first-line
	      (if (string-equal gnus-newsgroup-name "risks")
		  "In RISKS Digest,"
		(concat "In article " (mail-header-id message-reply-headers) ",")))
	     (his-address
	      (car (cdr (mail-extract-address-components
			 (mail-header-from message-reply-headers)))))
	    (second-line
	     (concat (if (string-equal his-address user-mail-address)
			 "I"
		       (mail-header-from message-reply-headers)) " wrote:\n")))
	(cond ((string-match "^rennlist\." gnus-newsgroup-name)
	       (insert second-line))
	      ((not (string-equal his-address "tickets@tickets.above.net"))
	       (insert first-line
		       (if (> (+ (length first-line) (length second-line)) fill-column)
			   "\n" " ")
		       second-line))))))

(setq nnweb-type 'altavista)

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
(setq bbdb/gnus-header-prefer-real-names t)
;; Not the same as (setq bbdb/gnus-mark-known-posters nil):
(setq bbdb/gnus-summary-known-poster-mark " ")
;; This affects only %uB; we get the info onscreen with %ub earlier in
;; the gnus-summary-line-format:
(setq bbdb/gnus-summary-mark-known-posters nil)

;; Mailcrypt, per the infopage:
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

(setq message-dont-reply-to-names "\\(m?shields@\\(msrl\\.com\\|mfnx\\.net\\|above\\.net\\|iad\\.above\\.net\\)\\|michael\\.shields@mmfn\\.com\\|seb@msrl\.com\\)")

;; Controls C-x m in message mode
(setq mail-user-agent 'message-user-agent)

(setq gnus-signature-limit 30.0)

;; Run auto spam complaint with `$' from summary buffer.
(autoload 'gnus-junk-complain "gnus-junk" "(not loaded yet)" t)
(add-hook 'gnus-summary-mode-hook
	  (lambda ()
	    (define-key gnus-summary-mode-map "$" 'gnus-junk-complain)))
(defadvice gnus-junk-check-hostname (around gnus-junk-no-raw-ip activate)
  "Don't send mail to abuse@ a raw IP address (e.g., abuse@172.16.0.1)."
  (cond ((not (ad-get-arg 0))
	 nil)
	((string-match "[a-zA-Z]" (ad-get-arg 0))
	 ad-do-it)
	(t nil)))

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
