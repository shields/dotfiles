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
(require 'seq)

(defface shields/whitespace '((t (:inherit fixed-pitch)))
  "Face for display-only whitespace markers.")
(defface shields/line-marker '((t (:inherit shields/whitespace)))
  "Face for line truncation and wrap markers.")
(defface shields/nobreak-marker '((t (:inherit fixed-pitch)))
  "Face for the marker of a no-break space with no trailing-space overlay.
A no-break space has none when it is not trailing whitespace, when its run of
trailing whitespace ends at point, and in a buffer where `whitespace-style' is
nil or `whitespace-mode' is off.")

(setq-default whitespace-style
              '(face tabs space-before-tab space-after-tab tab-mark
                missing-newline-at-eof))
(setopt whitespace-display-mappings
        (mapcar (lambda (mapping)
                  (if (eq (car mapping) 'tab-mark)
                      '(tab-mark ?\t [?⇥ ?\t])
                    mapping))
                (default-value 'whitespace-display-mappings)))

(defvar shields/tab-required-modes
  '(go-ts-mode go-mod-ts-mode go-work-ts-mode makefile-mode)
  "Major modes whose tabs should remain blank, including derived modes.")
(defvar-local shields/show-tabs t
  "Whether to mark tabs in buffers whose format does not require them.")
(put 'shields/show-tabs 'safe-local-variable #'booleanp)

(defvar-local shields/chosen-style nil
  "The `whitespace-style' that `shields/update-whitespace-style' set here.")

(defun shields/own-whitespace-style-p ()
  "Return non-nil if the buffer's `whitespace-style' is one this file chose.
That is the default, or what `shields/update-whitespace-style' set.  A style
that something else set for the buffer, such as a directory-local or file-local
variable, is not."
  (or (not (local-variable-p 'whitespace-style))
      (and shields/chosen-style
           (eq whitespace-style shields/chosen-style))))

(defun shields/update-whitespace-style ()
  "Choose tab visibility for the current buffer's format.
A `whitespace-style' that something else set for the buffer, such as a
directory-local or file-local variable, is kept."
  (when (shields/own-whitespace-style-p)
    (let ((style (if (or (not shields/show-tabs)
                         (derived-mode-p shields/tab-required-modes))
                     (seq-difference (default-value 'whitespace-style)
                                     '(tabs tab-mark))
                   (default-value 'whitespace-style)))
          (enabled whitespace-mode))
      (unless (equal style whitespace-style)
        (when enabled (whitespace-mode -1))
        (setq-local whitespace-style style)
        (setq shields/chosen-style style)
        (when enabled (whitespace-mode 1))))))
(add-hook 'after-change-major-mode-hook #'shields/update-whitespace-style -10)
(add-hook 'hack-local-variables-hook #'shields/update-whitespace-style)

;; Trailing-space markers are overlays, not a `display' property that font-lock
;; puts on the text.  Text properties belong to the text, which indirect buffers
;; share, so those buffers could not show or exempt markers independently, each
;; with its own point and `whitespace-mode' state.  Yanked or copied text would
;; also carry the property into other buffers.
(defvar-local shields/point nil
  "Marker at point as of the last command, which text inserted there advances.
JIT Lock functions can move point, so the markers consult this instead.")
(defvar-local shields/text-peers nil
  "Buffers sharing this buffer's text, or nil while JIT Lock alone suffices.")
(defvar-local shields/markers-complete nil
  "Whether this buffer's markers cover all its text, not only fontified text.")

(defconst shields/nobreak-spaces '(#xA0 #x202F)
  "The no-break spaces: U+00A0 and the narrow U+202F.")

(defconst shields/trailing-blanks (concat " \t" shields/nobreak-spaces)
  "Characters that make up trailing whitespace, in `skip-chars-forward' syntax.
They are the space, the tab, and the no-break spaces.")

(defun shields/fontify-trailing-spaces (start end)
  "Mark trailing spaces and no-break spaces on the lines between START and END.
Trailing whitespace is a run of spaces, tabs and no-break spaces that ends the
line.  Tabs get no overlay: `whitespace-mode' marks them, except where they
are blank on purpose (`shields/tab-required-modes', `shields/show-tabs').  As in
`whitespace-mode', leave unmarked a line whose run ends at point.  Return the
bounds of those lines, so that JIT Lock does not scan a long line again for each
chunk of it."
  (let ((point (marker-position shields/point))
        (regexp (concat "[" shields/trailing-blanks "]$")))
    (save-excursion
      (save-match-data
        (without-restriction
          ;; JIT Lock's END is exclusive, so a region that ends at a line start
          ;; leaves that line alone.
          (goto-char (if (> end start) (1- end) end))
          (setq end (pos-eol))
          (goto-char start)
          (setq start (pos-bol))
          ;; Through the newline: `replace-match' can stretch a marker over a
          ;; replacement that ends in one, and `remove-overlays' keeps the part
          ;; of an overlay that lies outside the range.
          (remove-overlays start (min (1+ end) (point-max))
                           'shields/trailing-space t)
          (goto-char start)
          (while (re-search-forward regexp end t)
            (let ((limit (point)))
              (unless (= limit point)
                (skip-chars-backward shields/trailing-blanks)
                (while (< (point) limit)
                  (let ((char (char-after)))
                    (unless (eq char ?\t)
                      ;; Front-advance keeps text typed before a blank out of
                      ;; its marker, which would hide the text and move point
                      ;; past the blank.  The low priority lets a selection
                      ;; show through the marker.  Each marker needs its own
                      ;; string: overlays that share one misplace the cursor.
                      (let ((overlay (make-overlay (point) (1+ (point)) nil t)))
                        (overlay-put overlay 'shields/trailing-space t)
                        (overlay-put overlay 'evaporate t)
                        (overlay-put overlay 'face 'shields/whitespace)
                        (overlay-put overlay 'priority -1)
                        (overlay-put overlay 'display
                                     (string
                                      (if (memq char shields/nobreak-spaces)
                                          ?⍽
                                        ?␣))))))
                  (forward-char)))))
          `(jit-lock-bounds ,start . ,(min (1+ end) (point-max))))))))

(defun shields/follow-point ()
  "Unmark the blanks that end at point, and mark those that did not."
  (let ((old (marker-position shields/point)))
    (unless (= old (point))
      (move-marker shields/point (point))
      (dolist (position (list old (point)))
        (when (save-excursion
                (without-restriction
                  (goto-char position)
                  (and (eolp)
                       (< (skip-chars-backward shields/trailing-blanks
                                               (1- (point)))
                          0))))
          (shields/fontify-trailing-spaces position position))))))

(defun shields/stop-trailing-space-markers ()
  "Remove this buffer's trailing-space markers and what maintains them."
  (jit-lock-unregister #'shields/fontify-trailing-spaces)
  (remove-hook 'post-command-hook #'shields/follow-point t)
  (remove-hook 'change-major-mode-hook #'shields/stop-trailing-space-markers t)
  (setq shields/point nil
        shields/markers-complete nil)
  (without-restriction
    (remove-overlays (point-min) (point-max) 'shields/trailing-space t)))

(defun shields/sync-trailing-space-markers (start end _length)
  "Refresh markers between START and END in the buffers sharing this text."
  (dolist (buffer shields/text-peers)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'shields/point buffer))
      (with-current-buffer buffer
        ;; Text that another buffer inserts at this buffer's point leaves point
        ;; in place but moves `shields/point' past the text.
        (shields/follow-point)
        (shields/fontify-trailing-spaces start end)))))

(defun shields/share-text (&optional leaving)
  "Mirror edits among the buffers sharing this text while one shows whitespace.
JIT Lock refontifies a buffer alone with its text.  LEAVING is a buffer that is
being killed."
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
         (buffers (seq-filter (lambda (buffer)
                                (and (not (eq buffer leaving))
                                     (eq base (or (buffer-base-buffer buffer)
                                                  buffer))))
                              (buffer-list)))
         (peers (and (cdr buffers)
                     (seq-some (lambda (buffer)
                                 (buffer-local-value 'shields/point buffer))
                               buffers)
                     buffers)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (or peers shields/text-peers)
          (setq shields/text-peers peers)
          (if peers
              (progn
                (add-hook 'after-change-functions
                          #'shields/sync-trailing-space-markers nil t)
                (add-hook 'kill-buffer-hook #'shields/leave-text nil t)
                ;; A sibling's JIT Lock marks shared text fontified, which
                ;; this buffer's would then skip.
                (when (and shields/point (not shields/markers-complete))
                  (setq shields/markers-complete t)
                  (without-restriction
                    (shields/fontify-trailing-spaces (point-min) (point-max)))))
            (remove-hook 'after-change-functions
                         #'shields/sync-trailing-space-markers t)
            (remove-hook 'kill-buffer-hook #'shields/leave-text t)
            (setq shields/markers-complete nil)))))))

(defun shields/leave-text ()
  "Stop mirroring edits into this buffer, which is being killed."
  (shields/share-text (current-buffer)))

(add-hook 'after-change-major-mode-hook #'shields/share-text 90)

(define-advice make-indirect-buffer
    (:filter-return (buffer) shields/share-new-buffer)
  "Mirror edits between BUFFER and the buffers it shares text with.
A buffer made indirect without cloning runs no mode hook, so nothing else
notices it."
  (with-current-buffer buffer
    (shields/share-text))
  buffer)

;; Remove trailing whitespace on save, for edited lines only.
(use-package ws-butler
  :config
  (ws-butler-global-mode 1))

;; `whitespace-style' has no `trailing' because the overlays above mark trailing
;; whitespace, but these commands read the style to decide what to clean up and
;; report, and `whitespace-action' calls them on save and on starting the mode.
(defun shields/with-trailing-style (function &rest arguments)
  "Call FUNCTION with ARGUMENTS, seeing `trailing' in this file's style.
In a buffer whose `whitespace-style' something else set (see
`shields/own-whitespace-style-p'), FUNCTION sees that style as given.  Only the
current buffer gets the added `trailing', and only while FUNCTION runs: the
default value and the other buffers keep what they have."
  (let ((style whitespace-style))
    (if (not (and style (shields/own-whitespace-style-p)))
        (apply function arguments)
      (let ((buffer (current-buffer))
            (style-local (local-variable-p 'whitespace-style))
            (active-local (local-variable-p 'whitespace-active-style))
            (trailing (cons 'trailing style)))
        (unwind-protect
            (progn
              ;; Binding a variable with no local value binds its default.
              (make-local-variable 'whitespace-style)
              (make-local-variable 'whitespace-active-style)
              (let ((whitespace-style trailing)
                    (whitespace-active-style
                     (if (eq whitespace-active-style style)
                         trailing
                       whitespace-active-style)))
                (apply function arguments)))
          (let ((inhibit-quit t))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (unless style-local
                  (kill-local-variable 'whitespace-style))
                (unless active-local
                  (kill-local-variable 'whitespace-active-style))))))))))
(dolist (command '(whitespace-cleanup whitespace-cleanup-region
                   whitespace-report whitespace-report-region))
  (advice-add command :around #'shields/with-trailing-style))

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

(defun shields/install-display-markers (table)
  "Set truncation, wrap and no-break space markers in display TABLE.
Leave alone a no-break space that TABLE already maps, as `whitespace-mode' does
for a `space-mark' in `whitespace-style'."
  (set-display-table-slot table 'truncation
                          (make-glyph-code ?… 'shields/line-marker))
  (set-display-table-slot table 'wrap
                          (make-glyph-code ?⸗ 'shields/line-marker))
  (dolist (char shields/nobreak-spaces)
    (unless (aref table char)
      (aset table char (vector (make-glyph-code ?⍽ 'shields/nobreak-marker))))))
(unless standard-display-table
  (setq standard-display-table (make-display-table)))
(shields/install-display-markers standard-display-table)
;; `whitespace-mode' copies the standard display table, which has our markers,
;; unless the buffer has a table of its own, as Org does when `org-ellipsis' is
;; set.  Add the markers to a copy of the buffer's table, because
;; `whitespace-mode' copies it only for a style with marks and Org shares one
;; table among its buffers.
(defun shields/whitespace-mode-markers ()
  "Keep display markers synchronized with `whitespace-mode'."
  (when (and whitespace-mode buffer-display-table)
    (setq buffer-display-table (copy-sequence buffer-display-table))
    (shields/install-display-markers buffer-display-table))
  (if (and whitespace-mode whitespace-active-style)
      (progn
        (setq shields/point (copy-marker (point) t))
        (add-hook 'post-command-hook #'shields/follow-point nil t)
        (add-hook 'change-major-mode-hook
                  #'shields/stop-trailing-space-markers nil t)
        ;; `jit-lock-register' fails in an indirect buffer, whose `fontified'
        ;; property is shared with its base: it prints a message and turns off
        ;; the JIT Lock that a clone inherits.
        (unless (buffer-base-buffer)
          (jit-lock-register #'shields/fontify-trailing-spaces)))
    (shields/stop-trailing-space-markers))
  (shields/share-text))
(add-hook 'whitespace-mode-hook #'shields/whitespace-mode-markers)
(add-hook 'clone-buffer-hook #'shields/whitespace-mode-markers)
(add-hook 'clone-indirect-buffer-hook #'shields/whitespace-mode-markers)
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

;; The theme is light, but its default face leaves a terminal frame's colors to
;; the terminal, so Emacs guesses the background from the terminal type: dark
;; under tmux and screen, which picks the dark variant of every face that has
;; one.
(setopt frame-background-mode 'light)

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
