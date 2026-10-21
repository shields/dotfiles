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
(menu-bar-mode -1)
;; Void in a build without a window system, as is `set-fringe-mode'.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

(require 'whitespace)
(require 'jit-lock)

(defface shields/whitespace '((t (:inherit fixed-pitch)))
  "Face for display-only whitespace markers.")
(defface shields/line-marker '((t (:inherit shields/whitespace)))
  "Face for line truncation and wrap markers.")

(setq-default whitespace-style '(face tabs tab-mark))
(setopt whitespace-display-mappings '((tab-mark ?\t [?⇥ ?\t])))

(defvar shields/tab-required-modes
  '(go-mode go-ts-mode go-mod-mode go-mod-ts-mode go-work-mode go-work-ts-mode
    makefile-mode makefile-ts-mode)
  "Major modes whose tabs should remain blank, including derived modes.")
(defvar-local shields/show-tabs t
  "Whether to mark tabs in buffers whose format does not require them.")
(put 'shields/show-tabs 'safe-local-variable #'booleanp)

(defun shields/update-whitespace-style ()
  "Choose tab visibility for the current buffer's format."
  (let ((enabled whitespace-mode))
    (when enabled (whitespace-mode -1))
    (setq-local whitespace-style
                (if (or (not shields/show-tabs)
                        (apply #'derived-mode-p shields/tab-required-modes))
                    '(face)
                  '(face tabs tab-mark)))
    (when enabled (whitespace-mode 1))))
(add-hook 'after-change-major-mode-hook #'shields/update-whitespace-style -10)
(add-hook 'hack-local-variables-hook #'shields/update-whitespace-style)

(defun shields/clear-trailing-space-markers ()
  "Remove this buffer's trailing-space overlays."
  (save-restriction
    (widen)
    (remove-overlays (point-min) (point-max) 'shields/trailing-space t)))

(defun shields/fontify-trailing-spaces (start end)
  "Mark trailing spaces on the lines between START and END."
  (save-excursion
    (save-match-data
      (save-restriction
        (widen)
        (goto-char end)
        (setq end (line-end-position))
        (goto-char start)
        (setq start (line-beginning-position))
        (remove-overlays start end 'shields/trailing-space t)
        (goto-char start)
        (while (re-search-forward "[ \t]+$" end t)
          (let ((limit (match-end 0)))
            (goto-char (match-beginning 0))
            (while (< (point) limit)
              (when (eq (char-after) ?\s)
                (let ((overlay (make-overlay (point) (1+ (point)))))
                  (overlay-put overlay 'shields/trailing-space t)
                  (overlay-put overlay 'evaporate t)
                  (overlay-put overlay 'face 'shields/whitespace)
                  (overlay-put overlay 'display (string ?␣))))
              (forward-char))))))))

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

(when (fboundp 'set-fringe-mode)
  (set-fringe-mode '(nil . 0)))         ; left-only

(setf (alist-get 'height default-frame-alist) 999)
(setf (alist-get 'width default-frame-alist) 132)
(setf (alist-get 'internal-border-width default-frame-alist) 0)

;; Enable color emoji.  `featurep' and not `display-graphic-p', because a macOS
;; daemon is headless when this runs.  `set-fontset-font' is void in a build
;; without a window system.
(when (featurep 'ns)
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

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

(setq-default truncate-lines nil
              truncate-partial-width-windows nil)

(defun shields/install-line-markers (table)
  "Set truncation and wrap markers in display TABLE."
  (set-display-table-slot table 'truncation
                          (make-glyph-code ?… 'shields/line-marker))
  (set-display-table-slot table 'wrap
                          (make-glyph-code ?⸗ 'shields/line-marker)))
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(shields/install-line-markers standard-display-table)
;; `whitespace-mode' installs a buffer-local display table for `tab-mark',
;; which shadows the standard one.  Reapply our markers there too.
(defun shields/whitespace-mode-markers ()
  "Keep display markers synchronized with `whitespace-mode'."
  (if whitespace-mode
      (progn
        (when buffer-display-table
          (shields/install-line-markers buffer-display-table))
        (add-hook 'change-major-mode-hook #'shields/clear-trailing-space-markers nil t)
        (jit-lock-register #'shields/fontify-trailing-spaces))
    (jit-lock-unregister #'shields/fontify-trailing-spaces)
    (remove-hook 'change-major-mode-hook #'shields/clear-trailing-space-markers t)
    (shields/clear-trailing-space-markers)))
(add-hook 'whitespace-mode-hook #'shields/whitespace-mode-markers)
(global-whitespace-mode 1)

;; Enable horizontal trackpad scrolling.
(setopt mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t)

;; Enable sub-line scrolling and window sizing.
(pixel-scroll-precision-mode t)
(setopt pixel-scroll-precision-use-momentum t)
(setopt frame-resize-pixelwise t)

(setopt switch-to-buffer-obey-display-actions t)
(setopt window-combination-resize t)

(setopt quit-restore-window-no-switch t
        frame-auto-hide-function #'delete-frame)

;; Old `custom-set-faces' calls left copies in `custom-file'.  The user theme
;; takes precedence, so discard those copies for faces now owned by shields.
;; Clear the saved metadata too, so the next Custom save drops the stale specs.
(load-theme 'shields t t)
;; The reset API takes (FACE IGNORED) pairs and only updates theme records.
;; Recalculate faces with `enable-theme' after all user overrides are gone.
(dolist (setting (get 'shields 'theme-settings))
  (when (eq (car setting) 'theme-face)
    (let ((face (cadr setting)))
      (custom-theme-reset-faces 'user (list face nil))
      (put face 'saved-face nil)
      (put face 'saved-face-comment nil))))
(enable-theme 'shields)

(provide 'init-display)
;;; init-display.el ends here
