;;; init-core.el --- Core Emacs configuration -*- lexical-binding: t -*-

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
;; Core initialization and preliminary settings

;;; Code:

;; Preliminaries
(when (eq system-type 'darwin)
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Write customizations to a separate file instead of appending here.
(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;; Un-disable some disabled commands. That concept is a mistake: if a command is
;; too surprising for casual use, then it should not be bound by default.
(put 'narrow-to-region 'disabled nil)   ; C-x n n
(put 'set-goal-column 'disabled nil)    ; C-x C-n

;; Stop saying "You can run the command blah-blah with M-x bl-b".
(setopt extended-command-suggest-shorter nil)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(provide 'init-core)
;;; init-core.el ends here
