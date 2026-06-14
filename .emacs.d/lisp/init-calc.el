;;; init-calc.el --- Calculator configuration -*- lexical-binding: t -*-

;; Copyright © 2003, 2026 Michael Shields
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
;; Configuration for Emacs' calculator

;;; Code:

;; Calc configuration
(setopt calc-group-char " ")
(setopt calc-date-format '(YYY "-" MM "-" DD (" " hh ":" mm ":" ss)))
(setopt calc-display-trail nil)

(setq math-additional-units
      '((fathom "6 * ft" "Fathom")
        (furlong "mi / 8" "Furlong")
        (fortnight "14 * day" "Fourteen nights")))

(eval-after-load "calc"
  '(progn
     (defun pop-up-calc ()
       "Create a new frame with Calc in it.
       The new frame has properties determined by calc-pop-up-frame-properties.
       This function is useful for binding to a hotkey."
       (interactive)
       (let ((frame (make-frame calc-pop-up-frame-properties))
             (buf (generate-new-buffer " pop-up-calc")))
         (select-frame-set-input-focus frame)
         (setq calc-transient-frames (cons frame calc-transient-frames))
         ;; If Calc starts up in its own buffer, it quits.  Hack around.
         (set-buffer buf)
         (full-calc)
         (kill-buffer buf)))
     (defvar calc-pop-up-frame-properties '(height 30 width 60)
       "Frame properties for frames created by \\[pop-up-calc].")
     (defvar calc-transient-frames ()
       "When calc-quit is run, the current frame will be deleted if it is in this list.")
     (defun calc-quit-or-delete-transient-frame (&optional non-fatal)
       "Deletes the current frame if it is a member of calc-transient-frames; otherwise, calc-quit."
       (interactive)
       (if (not (member (selected-frame) calc-transient-frames))
           (calc-quit non-fatal)
         (setq calc-transient-frames
               (delete (selected-frame) calc-transient-frames))
         (delete-frame)))
     (keymap-set calc-mode-map "q" 'calc-quit-or-delete-transient-frame)))

(provide 'init-calc)
;;; init-calc.el ends here
