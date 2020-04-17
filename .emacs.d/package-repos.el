(require 'package)

;; Set exactly this instead of using add-to-list, because the default
;; recklessly uses HTTP/80.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 50)
	("melpa" . 0)
	("melpa-stable" . 100)))

(setq package-pinned-packages
      '((flycheck . "melpa")))

(package-initialize)
