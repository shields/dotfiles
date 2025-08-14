;;; init-text-modes.el --- Text editing modes -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Configuration for text-based modes (markdown, text, etc.)

;;; Code:

;; Markdown
(use-package markdown-mode
  :hook
  (markdown-mode . variable-pitch-mode)

  :custom-face
  (markdown-code-face ((t (:inherit fixed-pitch :background "#f850f850f850" :height 0.8))))
  (markdown-header-face ((t (:weight bold)))))

;; JSON
(use-package jsonnet-mode)

;; YAML
(use-package yaml-mode
  :hook
  (yaml-ts-mode-hook . eglot-ensure))

;; Text mode and indented-text-mode
(setopt fill-column 80)

(setopt sentence-end-double-space nil)

;; Enable auto-fill in various modes
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'indented-text-mode-hook #'turn-on-auto-fill)
(add-hook 'message-mode-hook #'turn-on-auto-fill)
(add-hook 'xml-mode-hook #'turn-on-auto-fill)

;; Perl extension glues.  Not really like C; more like a Makefile.
(or (assoc "\\.xs$" auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.xs$" . indented-text-mode)))

;; View mode
(defun shields/goto-prefix-percent (arg)
  (interactive "NGoto percentage: ")
  (if (and (>= arg 0) (<= arg 100))
      (goto-char (+ (point-min)
                    (/ (* (- (point-max) (point-min)) arg) 100)))
    (error "No such thing as %d%%" arg)))

(use-package view
  :custom
  (view-read-only t)
  :bind
  (:map view-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("^" . beginning-of-line)
        ("$" . end-of-line)
        ("G" . goto-line)
        ("%" . shields/goto-prefix-percent)))

;; XML
(add-hook 'xml-mode-hook
          (lambda ()
            (keymap-set xml-mode-map "'"
                        (lambda ()
                          (interactive)
                          (insert-string "&#8217;")))))

;; Help mode
(setopt help-window-select t)

(add-to-list 'display-buffer-alist
             '("^\\*Help\\*"
               (display-buffer-reuse-mode-window
                display-buffer-below-selected)))

(keymap-set help-mode-map "q" #'quit-window)

(provide 'init-text-modes)
;;; init-text-modes.el ends here
