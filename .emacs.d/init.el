;;; init.el --- Shields's Emacs initialization file  -*- lexical-binding: t -*-

;; Copyright © 2001-2003, 2018, 2020-2025 Michael Shields
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
;; Main entry point for modular Emacs configuration

;;; Code:

;; Load core modules
(require 'init-core)
(require 'init-display)
(require 'init-editing)  ;; Includes minor modes
(require 'init-modeline)
(require 'init-files)
(require 'init-keybindings)

;; Programming languages and modes
(require 'init-eglot)    ;; LSP client configuration
(require 'init-prog-modes)  ;; Programming language modes

;; Text editing modes
(require 'init-text-modes)

;; Tools and features
(require 'init-magit)    ;; Git interface
(require 'init-completion)  ;; Completion framework
(require 'init-terminal)  ;; Terminal emulation
(require 'init-dired)    ;; Directory editor
(require 'init-search)   ;; Search tools
(require 'init-compile)  ;; Compilation mode
(require 'init-debug)    ;; Debugging tools
(require 'init-spell)    ;; Spell checking
(require 'init-doc)      ;; Documentation and help
(require 'init-ai)       ;; AI and LLM tools
(require 'init-calc)     ;; Calculator
(require 'init-calendar) ;; Calendar
(require 'init-extras)   ;; Miscellaneous utilities

;; Provide this module
(provide 'init)
;;; init.el ends here
