;;; init-files.el --- File and buffer handling -*- lexical-binding: t -*-

;; Copyright © 2003, 2020, 2025-2026 Michael Shields
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
;; Settings for file and buffer management

;;; Code:

;; Make sure editing a hard-linked file edits all its links.
(setopt backup-by-copying-when-linked t)

;; Only switch to existing buffers interactively
(defun shields/switch-to-buffer-existing-only (orig-fun &rest args)
  "When called interactively switch to existing buffers only, unless
when called with a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer)
                      (null current-prefix-arg))))
  (apply orig-fun args))

(advice-add 'switch-to-buffer :around #'shields/switch-to-buffer-existing-only)

(setopt auto-save-default nil)
(setopt make-backup-files nil)
(setopt create-lockfiles nil)

(global-auto-revert-mode 1)
(setopt global-auto-revert-non-file-buffers t)
(setopt auto-revert-verbose nil)

;; This is only an issue when looking at the source for Emacs Lisp packages
;; installed via straight.
(setopt vc-follow-symlinks t)

;; Auto-chmod +x scripts with shebangs on save.
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Don't let ffap probe the network for hostnames under point.
(setq ffap-machine-p-known 'reject)

(provide 'init-files)
;;; init-files.el ends here
