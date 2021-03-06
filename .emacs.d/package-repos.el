(require 'package)

;; Set exactly this instead of using add-to-list, because the default
;; recklessly uses HTTP/80.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 0)
	("melpa" . 50)))

(package-initialize)
