(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bbdb-complete-name-allow-cycling t)
 '(bbdb-gui nil)
 '(bbdb-time-display-format "%Y-%m-%d")
 '(canlock-password "c5050b0b84d19f7fc1b4a113f2fbbab44c219402")
 '(gnus-treat-body-boundary nil)
 '(gnus-treat-display-xface nil)
 '(gnus-treat-from-picon nil)
 '(gnus-treat-mail-picon nil)
 '(gnus-treat-newsgroups-picon nil)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(message-required-mail-headers
   (quote
    (From Date
	  (optional . In-Reply-To)
	  Message-ID Lines
	  (optional . User-Agent))))
 '(package-selected-packages
   (quote
    (projectile go-mode company yasnippet smartparens markdown-mode magit go-eldoc flycheck company-go aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnus-cite-face-4 ((((class color) (background light)) (:foreground "slateblue3"))))
 '(gnus-group-mail-3-empty-face ((((class color) (background light)) nil)))
 '(gnus-group-mail-3-face ((((class color) (background light)) nil)))
 '(gnus-group-news-3-face ((((class color) (background light)) nil)))
 '(gnus-group-news-4-face ((((class color) (background light)) nil)))
 '(gnus-group-news-5-face ((((class color) (background light)) nil)))
 '(gnus-group-news-6-face ((((class color) (background light)) nil)))
 '(gnus-header-content-face ((((class color) (background light)) (:foreground "indianred4"))))
 '(gnus-header-newsgroups-face ((((class color) (background light)) (:foreground "MidnightBlue"))))
 '(gnus-signature-face ((((type x)) (:foreground "grey50"))))
 '(gnus-summary-low-ancient-face ((((class color) (background light)) (:foreground "RoyalBlue"))))
 '(gnus-summary-low-read-face ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(gnus-summary-low-ticked-face ((((class color) (background light)) (:foreground "firebrick"))))
 '(gnus-summary-low-unread-face ((t (:foreground "grey18"))))
 '(info-node ((t (:foreground "darkblue"))))
 '(message-cited-text-face ((((class color) (background light)) (:foreground "indianred4"))) t))

(setq minibuffer-max-depth nil)
