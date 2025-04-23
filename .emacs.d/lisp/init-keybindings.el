;;; init-keybindings.el --- Global key bindings -*- lexical-binding: t -*-

;; Author: Michael Shields <shields@msrl.com>

;;; Commentary:
;; Global keyboard shortcuts and key binding configuration

;;; Code:

(defun shields/delete-window-or-frame ()
  "Call `delete-window'. If it fails, call `delete-frame'."
  (interactive)
  (condition-case nil
      (delete-window)
    (error (delete-frame))))

;; macOS modifier key setup
;; Option (or Alt) ⌥: ignore so as to allow system-wide symbol input mechanism
(setopt ns-alternate-modifier nil)
(setopt ns-right-alternate-modifier nil)
;; Command (or Cmd) ⌘
(setopt ns-command-modifier 'meta)
(setopt ns-right-command-modifier 'meta)

;; macOS only supports four modifiers, but we use Karabiner elements to map two
;; additional keys to F24, then set them to a non-repeating `C-x @ s', in Emacs
;; only, to emulate Super.

;; Standard macOS shortcuts - https://support.apple.com/en-us/102650
;; See also ns-win.el
(keymap-global-set "M-x" #'kill-region)
(keymap-global-set "s-x" #'execute-extended-command)
(keymap-global-set "M-c" #'copy-region-as-kill)
(keymap-global-set "M-v" #'yank)
(keymap-global-set "M-z" #'undo)
(keymap-global-set "M-a" #'mark-whole-buffer)
(keymap-global-set "M-f" #'isearch-forward)
(keymap-global-set "M-w" #'shields/delete-window-or-frame)
(with-eval-after-load 'isearch
  (define-key isearch-mode-map [(meta g)] #'isearch-repeat-forward))

;; Navigation
(keymap-global-set "M-`" #'other-frame)
(keymap-global-set "M-'" #'next-multiframe-window)
(keymap-global-set "M-\"" #'previous-multiframe-window)
(keymap-global-set "M-n" #'next-error)
(keymap-global-set "M-p" #'previous-error)
(keymap-global-set "M-t" #'previous-buffer)
(keymap-global-set "M-T" #'next-buffer)
(keymap-global-set "C-t" #'shields/maybe-project-switch-to-buffer)

;; Coding
(keymap-global-set "M-/" #'completion-at-point)
(keymap-global-set "M-:" #'comment-dwim)
(keymap-global-set "C-c F" #'find-file-at-point)
(keymap-global-set "s-SPC" #'fixup-whitespace)
(keymap-global-set "C-<backspace>" #'join-line)
(keymap-global-set "M-g" #'grep)
(keymap-global-set "M-r" #'replace-string)

;; Put M-ESC (i.e., ESC ESC) back to the way it was when I learned
;; Emacs.  Apparently this changed in 1994.
(keymap-global-set "M-ESC" #'eval-expression)

;; Disable some keys
(keymap-global-unset "C-x f")  ; set-fill-column
(keymap-global-unset "C-x o")  ; other-window
(keymap-global-unset "C-v")    ; scroll-up-command
(keymap-global-unset "C-w")    ; kill-region
(keymap-global-unset "M-q")    ; macOS standard to quit
;; macOS standard bindings from ns-win.el that I just don't like:
(keymap-global-unset "s-m")    ; iconify-frame
(keymap-global-unset "s-q")    ; save-buffers-kill-emacs
(keymap-global-unset "s-t")    ; menu-set-font
(keymap-global-unset "C-<mouse-4>")     ; mouse-wheel-text-scale
(keymap-global-unset "C-<mouse-5>")     ; mouse-wheel-text-scale
(keymap-global-unset "C-<wheel-down>")  ; mouse-wheel-text-scale
(keymap-global-unset "C-<wheel-up>")    ; mouse-wheel-text-scale

;; Disable bindings for the secondary selection, often activated by mistake and
;; never useful.
(keymap-global-unset "M-<mouse-1>")
(keymap-global-unset "M-<mouse-2>")
(keymap-global-unset "M-<mouse-3>")
(keymap-global-unset "M-<down-mouse-1>")
(keymap-global-unset "M-<drag-mouse-1>")
(keymap-global-unset "s-y")

(use-package xref
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-pop-marker-stack)))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(keymap-global-set "M-<up>" #'move-line-up)
(keymap-global-set "M-<down>" #'move-line-down)

(use-package project
  :bind ("M-o" . shields/maybe-project-find-file))    ; Really M-b, remapped by Karabiner

(defun shields/escape-project-find-file ()
  "Exit project-find-file and run find-file."
  (interactive)
  (run-at-time 0.01 nil (lambda () (call-interactively #'find-file)))
  (exit-minibuffer))

(defun shields/maybe-project-find-file (arg)
  "Open in the current project if in a project, otherwise whatever.
If \\<global-map>\\[shields/maybe-project-find-file] is pressed inside project-find-file minibuffer, abort and run find-file."
  (interactive "P")
  (if (project-current)
      (minibuffer-with-setup-hook
          (lambda ()
            (local-set-key (kbd "M-o") #'shields/escape-project-find-file))
        (project-find-file arg))
    (call-interactively #'find-file)))

(defun shields/escape-project-switch-to-buffer ()
  "Exit project-switch-to-buffer and run switch-to-buffer."
  (interactive)
  (run-at-time 0.01 nil (lambda () (call-interactively #'switch-to-buffer)))
  (exit-minibuffer))

(defun shields/maybe-project-switch-to-buffer ()
  "Switch to buffer in the current project if in a project, otherwise whatever.
If \\<global-map>\\[shields/maybe-project-switch-to-buffer] is pressed inside project-switch-to-buffer minibuffer, abort and run switch-to-buffer."
  (interactive)
  (if (project-current)
      (minibuffer-with-setup-hook
          (lambda ()
            (local-set-key (kbd "C-t") #'shields/escape-project-switch-to-buffer))
        (call-interactively #'project-switch-to-buffer))
    (call-interactively #'switch-to-buffer)))

(defun shields/ffap-at-mouse-error ()
  "Same behavior as default, except an error instead of a message."
  (interactive)
  (error "No file or URL found at mouse click."))

(setq ffap-at-mouse-fallback #'shields/ffap-at-mouse-error)

(keymap-global-set "M-<mouse-1>" #'ffap-at-mouse)

(defun shields/save-dwim (arg)
  "Save and do other things.

If the file is part of an active server edit or `with-editor' session,
then finish that.

If the file is being freshly saved and it is part of a project,
also save all other project buffers.

If the file was already saved and it is part of a Magit repo,
stage it and display a diff."
  (interactive "P")
  (if (buffer-modified-p)
      ;; File is being freshly saved.
      (progn
        (save-buffer)
        (when-let* ((proj (project-current)))
          (let ((inhibit-message t))
            (dolist (buf (project-buffers proj))
              (with-current-buffer buf
                (when (and (buffer-file-name)
                           (buffer-modified-p))
                  (save-buffer)))))))
    ;; File was already saved. If Magit tracks it, then stage it.
    (when (and (buffer-file-name)
               (magit-file-tracked-p (buffer-file-name)))
      (magit-file-stage)
      (magit-diff-buffer-file)))
  ;; Finish server edit if applicable, whether or not we saved any
  ;; modifications.
  (cond (with-editor-mode
         (with-editor-finish arg))
        (server-buffer-clients
         (server-edit))))

(keymap-global-set "M-s" #'shields/save-dwim)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
