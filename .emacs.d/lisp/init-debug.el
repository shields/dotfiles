;;; init-debug.el --- Debugging tools -*- lexical-binding: t -*-

;; Copyright © 2020 Michael Shields
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
;; Configuration for debugging, error checking, and tracing

;;; Code:

;; Debug Adapter Protocol
(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)

  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)

  (require 'dap-go)
  (dap-go-setup)

  (require 'dap-python))

;; Flymake configuration for errors and warnings
(defun shields/clean-flymake-diagnostic-message (message)
  (pcase message
    ((rx bos (or "pyright" "basedpyright") " [" (+ (not "]")) "]: " (let msg (+ anything)) eos)
     msg)
    ((rx bos "Ruff: " (+ (any "A-Z0-9")) " " (let msg (+ anything)) eos)
     msg)
    (_ message)))

(defun shields/flymake-make-diagnostic-advice (args)
  (pcase-let* ((`(,locus ,beg ,end ,type ,text ,data ,overlay-properties) args)
               (cleaned-text (shields/clean-flymake-diagnostic-message text)))
    (list locus beg end type cleaned-text data overlay-properties)))

(ert-deftest shields/test-clean-flymake-diagnostic-message ()
  (should (string=
           (shields/clean-flymake-diagnostic-message "pyright [reportUnknownVariableType]: Type of \"i\" is unknown")
           "Type of \"i\" is unknown"))
  (should (string=
           (shields/clean-flymake-diagnostic-message "basedpyright [reportUnknownVariableType]: Type of \"j\" is unknown")
           "Type of \"j\" is unknown"))
  (should (string=
           (shields/clean-flymake-diagnostic-message "Ruff: F821 Undefined name `y`")
           "Undefined name `y`"))
  (should (string=
           (shields/clean-flymake-diagnostic-message "This is a normal message")
           "This is a normal message")))

(use-package flymake
  :config
  (advice-add 'flymake-make-diagnostic :filter-args #'shields/flymake-make-diagnostic-advice)

  :custom-face
  (flymake-end-of-line-diagnostics-face ((t (:background "gray97" :height 0.707))))
  (flymake-error-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-error) :weight normal))))
  (flymake-note-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-info) :weight normal))))
  (flymake-warning-echo-at-eol ((t (:inherit (flymake-end-of-line-diagnostics-face compilation-warning) :weight normal)))))

(require 'edebug)

(provide 'init-debug)
;;; init-debug.el ends here
