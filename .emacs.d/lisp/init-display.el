;;; init-display.el --- Display configuration -*- lexical-binding: t -*-

;; Copyright © 2003, 2020-2021, 2024-2026 Michael Shields
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;; Visual appearance settings including fonts, colors, and UI elements

;;; Code:

(use-package hl-todo
  :config
  (global-hl-todo-mode 1)
  (setopt hl-todo-keyword-faces
          '(("FIXME" . "#ff0000")
            ("XXX+"  . "#ff0000"))))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (setopt symbol-overlay-idle-time 0.1)
  :custom-face
  (symbol-overlay-default-face ((t (:foreground "magenta")))))

;; Basic interface settings
(setopt inhibit-startup-message t
        initial-scratch-message nil
        window-min-height 2
        blink-matching-delay 0.25
        tab-bar-show 1
        tab-bar-close-last-tab-choice 'delete-frame
        use-dialog-box nil)

;; Disable various visual elements
(blink-cursor-mode 0)
(menu-bar-mode (if (eq system-type 'darwin) -1 1))
(tool-bar-mode 0)

;; Highlight tabs and trailing spaces
(setq-default whitespace-style
              '(face
                tabs trailing space-before-tab space-after-tab tab-mark
                missing-newline-at-eof))
(global-whitespace-mode 1)
(defun shields/suppress-whitespace-mode ()
  (setq-local whitespace-style nil))

;; Remove trailing whitespace on save, for edited lines only.
(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

;; Enable visual bell.  But on macOS, the visual bell pops up "the
;; standard NextStep image 'caution'" (src/nsterm.m).  This is not
;; correct.  Better is to set "Flash the screen when an alert sound
;; occurs" in Accessibility preferences.
(defun macos-system-alert ()
  "Make the systemwide alert event (sound or screen flash)."
  (do-applescript "tell application \"System Events\" to beep"))
;; This needs to check system-type and not window-system because an Emacs daemon
;; started as a macOS login item is headless.
(cond ((eq system-type 'darwin)
       (setopt visible-bell nil)
       (setopt ring-bell-function 'macos-system-alert))
      (t
       (setopt visible-bell t)))

(set-fringe-mode '(nil . 0))            ; left-only

(setf (alist-get 'height default-frame-alist) 999)
(setf (alist-get 'width default-frame-alist) 132)
(setf (alist-get 'internal-border-width default-frame-alist) 0)

;; Enable color emoji.
(set-fontset-font
 t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(use-package diff-hl
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh))

;; Enable smartparens.  Note that it requires configuration, and that
;; a stock configuration is provided by smartparens-config.  If you
;; just let it autoload, it will work, but not well.
(use-package smartparens
  :demand t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :custom
  (sp-show-pair-delay 0)
  (sp-ignore-modes-list nil)            ; Even the minibuffer!
  :custom-face
  (sp-pair-overlay-face ((t (:inherit sp-show-pair-match-content-face))))
  (sp-show-pair-match-content-face ((t (:inherit show-paren-match-expression))))
  (sp-show-pair-match-face ((t (:inherit (show-paren-match show-paren-match-expression)))))
  (sp-wrap-overlay-opening-pair ((t (:inherit sp-wrap-overlay-face :foreground "magenta")))))

(setopt scroll-error-top-bottom t)

(setq-default truncate-lines t)

;; Markers shown when the right fringe is disabled (see above).  The family
;; override is required because display-table glyphs attached to a face are
;; resolved against the face's primary font directly, bypassing the fontset
;; fallback that handles missing glyphs in normal text.  Without it the face
;; inherits the default font (Andale Mono, which lacks U+2E17 DOUBLE OBLIQUE
;; HYPHEN ⸗) and the wrap marker shows as a missing-glyph box -- even though
;; ⸗ renders fine inline because the fallback covers it.
(defface shields/line-marker '((t :family "Courier New" :background "#e8f4ff"))
  "Face for line truncation and wrap markers.")
(defun shields/install-line-markers (table)
  "Set truncation and wrap markers in display TABLE."
  (set-display-table-slot table 'truncation
                          (make-glyph-code ?… 'shields/line-marker))
  ;; The double oblique hyphen is what Gutenberg used.
  (set-display-table-slot table 'wrap
                          (make-glyph-code ?⸗ 'shields/line-marker)))
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(shields/install-line-markers standard-display-table)
;; `whitespace-mode' installs a buffer-local display table for `tab-mark',
;; which shadows the standard one.  Reapply our markers there too.
(defun shields/whitespace-mode-line-markers ()
  "Reapply line markers after `whitespace-mode' installs its table."
  (when (and whitespace-mode buffer-display-table)
    (shields/install-line-markers buffer-display-table)))
(add-hook 'whitespace-mode-hook #'shields/whitespace-mode-line-markers)

;; Enable horizontal trackpad scrolling.
(setopt mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t)

;; Enable sub-line scrolling and window sizing.
(pixel-scroll-precision-mode t)
(setopt pixel-scroll-precision-use-momentum t)
(setopt frame-resize-pixelwise t)

(setopt switch-to-buffer-obey-display-actions t)
(setopt window-combination-resize t)

;; Close frame when quitting the last window
(defun shields/quit-window-or-frame (orig-fun &optional kill window)
  "Quit WINDOW, or close frame if it's the only window.
This is advice for `quit-window' that closes the frame
when quitting the last window in the frame.

Uses advice rather than key remapping because many functions
(e.g., magit-mode-quit-window) wrap quit-window programmatically."
  (let ((window (or window (selected-window))))
    (if (one-window-p t)
        (delete-frame)
      (funcall orig-fun kill window))))

(advice-add 'quit-window :around #'shields/quit-window-or-frame)

;; Core Emacs custom faces
(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "AndaleMono Nerd Font"))))
 '(Info-quoted ((t (:inherit fixed-pitch))))
 '(completions-annotations ((t (:inherit shadow))))
 '(cursor ((t (:background "firebrick"))))
 '(fixed-pitch ((t (:family "AndaleMono Nerd Font"))))
 '(font-lock-builtin-face ((t (:inherit font-lock-function-call-face))))
 '(font-lock-comment-face ((t (:foreground "#197019"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-markup-face ((t (:inherit font-lock-doc-face))))
 '(font-lock-escape-face ((t (:underline t))))
 '(font-lock-function-call-face ((t (:foreground "#701919"))))
 '(font-lock-function-name-face ((t (:inherit font-lock-function-call-face :weight bold))))
 '(font-lock-keyword-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-negation-char-face ((t (:foreground "dark red"))))
 '(font-lock-number-face ((t (:inherit font-lock-string-face))))
 '(font-lock-property-name-face ((t (:inherit font-lock-property-use-face :weight bold))))
 '(font-lock-property-use-face ((t (:foreground "MidnightBlue"))))
 '(font-lock-punctuation-face ((t (:foreground "gray50"))))
 '(font-lock-reference-face ((t (:foreground "OrangeRed"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-escape-face))))
 '(font-lock-regexp-grouping-construct ((t (:inherit font-lock-escape-face))))
 '(font-lock-string-face ((t (:foreground "#005462"))))
 '(font-lock-type-face ((t (:foreground "#7070c2"))))
 '(font-lock-variable-name-face ((t (:weight bold))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face :weight normal))))
 '(font-lock-warning-face ((t (:inherit nil))))
 '(highlight ((t (:background "darkseagreen1"))))
 '(isearch ((t (:inherit match))))
 '(lazy-highlight ((t (:inherit match))))
 '(link-visited ((t (:inherit link))))
 '(lsp-face-highlight-textual ((t (:background "#d0ffd0"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#f850f850f850" :height 0.8))))
 '(markdown-header-face ((t (:weight bold))))
 '(match ((t (:background "findHighlightColor" :foreground "black"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(mode-line ((t (:background "#005462" :foreground "white" :family "Avenir Next"))))
 '(mode-line-buffer-id ((t (:weight semi-bold))))
 '(mode-line-emphasis ((t (:weight semi-bold))))
 '(mode-line-highlight ((t (:background "#0093a9"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#484848"))))
 '(parenthesis ((t (:inherit font-lock-bracket-face))))
 '(region ((t (:background "selectedTextBackgroundColor" :extend nil))))
 '(show-paren-match ((t (:foreground "magenta" :weight bold))))
 '(show-paren-match-expression ((t (:background "#f4f4ff"))))
 '(variable-pitch ((t (:height 1.2 :family "Avenir Next")))))

(provide 'init-display)
;;; init-display.el ends here
