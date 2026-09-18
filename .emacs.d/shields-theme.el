;;; shields-theme.el --- Shields's faces -*- lexical-binding: t -*-

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
;; These faces are a theme because Custom treats a face set with
;; `custom-set-faces' as saved by the user: the next time it saves anything, it
;; copies every such face into `custom-file', which loads before the init
;; files, and it never refreshes that copy.  It writes no theme's faces there.
;;
;; This file is directly in `user-emacs-directory' because that is the default
;; `custom-theme-directory', where `load-theme' looks.  It does not search
;; `load-path', so it would not find this file in lisp/.

;;; Code:

(deftheme shields
  "Shields's faces.")

;; The default face names its font with :font, not :family: set-face-attribute
;; applies :family before the other attributes and the default face reloads the
;; frame font right then, so the family is looked up at the face's previous
;; weight (normal), which the weight-350 CommitMonoShields build does not have,
;; and Emacs silently falls back to Helvetica.  A :font without a size leaves
;; the size to :height.  fixed-pitch keeps :family: other faces are realized
;; lazily with weight, slant and width dropped from the lookup (bug#5934), so
;; the family alone finds the nearest weight, whereas :font would pin semi-light
;; and upright on every face that inherits fixed-pitch and strip their bold and
;; italic.
;;
;; By default each call or use face inherits from its name face, and escape
;; from regexp-grouping-backslash.  The function, property, and escape pairs
;; repeat their attributes here and inherit in neither direction: Emacs 31
;; signals an error on even a momentary inheritance cycle, `enable-theme'
;; applies these faces one at a time, and until then the other face of a pair
;; has its default or a spec from `custom-file', which can inherit either way.
;;
;; reset is a pseudo-value that any attribute accepts, meaning the default
;; face's value, which for :weight is not normal here.
(custom-theme-set-faces
 'shields
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-light :height 120 :width normal :font "CommitMonoShields Nerd Font"))))
 '(Info-quoted ((t (:inherit fixed-pitch))))
 '(completions-annotations ((t (:inherit shadow))))
 '(cperl-array-face ((t (:foreground "Blue"))))
 '(cperl-hash-face ((t (:foreground "Red" :weight bold))))
 '(cursor ((t (:background "firebrick"))))
 '(fixed-pitch ((t (:family "CommitMonoShields Nerd Font"))))
 '(font-lock-builtin-face ((t (:inherit font-lock-function-call-face))))
 '(font-lock-comment-face ((t (:foreground "#197019"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-markup-face ((t (:inherit font-lock-doc-face))))
 '(font-lock-escape-face ((t (:underline t))))
 '(font-lock-function-call-face ((t (:foreground "#701919"))))
 '(font-lock-function-name-face ((t (:foreground "#701919" :weight bold))))
 '(font-lock-keyword-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-negation-char-face ((t (:foreground "dark red"))))
 '(font-lock-number-face ((t (:inherit font-lock-string-face))))
 '(font-lock-property-name-face ((t (:foreground "MidnightBlue" :weight bold))))
 '(font-lock-property-use-face ((t (:foreground "MidnightBlue"))))
 '(font-lock-punctuation-face ((t (:foreground "gray50"))))
 '(font-lock-reference-face ((t (:foreground "OrangeRed"))))
 '(font-lock-regexp-grouping-backslash ((t (:underline t))))
 '(font-lock-regexp-grouping-construct ((t (:inherit font-lock-escape-face))))
 '(font-lock-string-face ((t (:foreground "#005462"))))
 '(font-lock-type-face ((t (:foreground "#7070c2"))))
 '(font-lock-variable-name-face ((t (:weight bold))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face :weight reset))))
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

(provide-theme 'shields)

;;; shields-theme.el ends here
