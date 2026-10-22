# Copyright © 2026 Michael Shields
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Homebrew's Linux build has no window system and no SQLite. No packages are
# installed here, so the harness records which use-package forms run: the `when'
# guards around them keep the Mac-only packages off Linux. The rest of each file
# runs for real, so an unguarded call to a function that the Linux build lacks
# fails the load. A :config body runs only for a package whose feature the
# harness provides, as it does for chatgpt-shell.

import json
import os
import re
import shutil
import subprocess
from pathlib import Path
from typing import TYPE_CHECKING, cast

import pytest

if TYPE_CHECKING:
    from collections.abc import Mapping

REPO = Path(__file__).resolve().parents[1]
EMACS_DIR = REPO / ".emacs.d"

FILES = sorted(path.stem for path in (EMACS_DIR / "lisp").glob("*.el"))

# atomic-chrome, Dash.app and the login shell's PATH need a Mac; forge needs
# SQLite.
GATED_PACKAGES = {"atomic-chrome", "dash-at-point", "exec-path-from-shell", "forge"}

TERMINAL_KEYS = {
    "C-c i": "imenu",
    "C-c SPC": "fixup-whitespace",
    "C-c o": "crux-smart-open-line",
    "C-c .": "goto-last-change",
    "C-c ,": "goto-last-change-reverse",
    "C-c j": "avy-goto-char-timer",
    "C-c =": "expreg-expand",
    "C-c -": "expreg-contract",
    "C-c J": "join-line",
}
GRAPHICAL_KEYS = {
    "s-i": "C-c i",
    "s-SPC": "C-c SPC",
    "s-o": "C-c o",
    "s-.": "C-c .",
    "s-,": "C-c ,",
    "<f19>": "C-c j",
    "C-=": "C-c =",
    "C--": "C-c -",
    "C-<backspace>": "C-c J",
}
# Other C-c keys, which the terminal twins must not displace.
OTHER_KEYS = {
    "C-c d": "crux-duplicate-current-line-or-region",
    "C-c e": "crux-eval-and-replace",
    "C-c F": "find-file-at-point",
}

HARNESS_SETUP = r""";;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'use-package)

(defvar h-repo (getenv "DOTFILES_EMACS_DIR"))
(defvar h-macos (equal (getenv "DOTFILES_EMACS_SCENARIO") "macos"))

(when (< emacs-major-version 31)
  (error "These files need Emacs 31, not %s" emacs-version))

;; init-editing.el turns JIT compilation back on, so deny every file too.
(setq native-comp-jit-compilation nil
      native-comp-jit-compilation-deny-list '(".")
      native-comp-enable-subr-trampolines nil
      user-emacs-directory (file-name-as-directory (getenv "HOME"))
      custom-theme-directory h-repo)
(add-to-list 'load-path (expand-file-name "lisp" h-repo))
(add-to-list 'custom-theme-load-path h-repo)

;; Batch Emacs starts with the menu bar off, so turn it on for init to turn off.
(menu-bar-mode 1)

(setq use-package-always-defer t)
(push :straight use-package-keywords)
(defun use-package-normalize/:straight (_name _keyword args) args)
(defun use-package-handler/:straight (name _keyword _arg rest state)
  (use-package-process-keywords name rest state))
(defvar h-used nil)
(advice-add 'use-package :around
            (lambda (orig name &rest args)
              `(progn (push ',name h-used) ,(apply orig name args))))

(defvar h-calls nil)
(dolist (feature '(smartparens smartparens-config gcmh atomic-chrome))
  (provide feature))
(dolist (fn '(smartparens-global-mode show-smartparens-global-mode gcmh-mode
              atomic-chrome-start-server fancy-compilation-mode
              exec-path-from-shell-initialize))
  (let ((fn fn))
    (defalias fn (lambda (&rest _) (push fn h-calls)))))

(defcustom chatgpt-shell-anthropic-key "unset" "" :type '(choice function string))
(defcustom chatgpt-shell-openai-key "unset" "" :type '(choice function string))
(defun auth-source-pick-first-password (&rest args)
  (and (equal (plist-get args :host) "api.anthropic.com") "sk-ant"))
(defun chatgpt-shell--fader-palette ()
  (color-name-to-rgb (or (face-background 'default) "#1a1a1a")))
(provide 'chatgpt-shell)

;; The Mac build preloads tool-bar.el, but the Linux build loads it on demand,
;; and loading it later would replace the stub or define what the scenario
;; leaves void.
(require 'tool-bar)
(dolist (fn '(tool-bar-mode set-fringe-mode set-fontset-font))
  (let ((fn fn))
    (if h-macos
        (defalias fn (lambda (&rest _) (push fn h-calls)))
      (fmakunbound fn))))
(if h-macos
    (progn (setq system-type 'darwin) (provide 'ns))
  (setq system-type 'gnu/linux
        features (delq 'ns features)))
(fset 'sqlite-available-p (lambda () h-macos))

(defvar h-lookups 0)
(defun magit-config-get-from-cached-list (_key)
  (cl-incf h-lookups)
  "octocat")
(when h-macos (defvar forge-owned-accounts nil))

(defvar h-loaded nil)
(dolist (file (split-string (getenv "DOTFILES_EMACS_FILES")))
  (load (expand-file-name (concat file ".el") (expand-file-name "lisp" h-repo))
        nil t t)
  (push file h-loaded))

(require 'eglot)
(require 'term/tmux)
"""

HARNESS_REPORT = r"""
(defun h-vec (list)
  (vconcat (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x)) list)))

;; use-package's :custom sets a variable only once its own theme is enabled.
(defun h-themed (variable)
  (eval (cadr (assq 'use-package (get variable 'theme-value))) t))

(defun h-key (key)
  (let ((binding (key-binding (kbd key))))
    (if (and (symbolp binding) (fboundp binding))
        (symbol-name binding)
      (format "not a command: %S" binding))))

(defun h-face (face attributes)
  (let ((spec (cadr (assq 'shields (get face 'theme-face)))))
    (cl-flet ((pick (plist)
                (mapcar (lambda (a) (cons a (plist-get plist a))) attributes)))
      `((ns . ,(pick (cadr (assoc '((type ns)) spec))))
        (other . ,(pick (cadr (assq t spec))))
        (here . ,(pick (face-spec-choose spec)))))))

(defun h-default-colors (window-system)
  "Return the colors that the default face's theme spec gives WINDOW-SYSTEM."
  (cl-letf (((symbol-function 'window-system) (lambda (&optional _) window-system)))
    (let ((attributes (face-spec-choose
                       (cadr (assq 'shields (get 'default 'theme-face))))))
      `((background . ,(plist-get attributes :background))
        (foreground . ,(plist-get attributes :foreground))))))

(defun h-fader-end (&optional background)
  "Return the color that `chatgpt-shell--fader-palette' fades to, as hex.
BACKGROUND, if given, is what `face-background' returns."
  (let ((rgb (if background
                 (cl-letf (((symbol-function 'face-background)
                            (lambda (&rest _) background)))
                   (chatgpt-shell--fader-palette))
               (chatgpt-shell--fader-palette))))
    (and rgb (apply #'color-rgb-to-hex (append rgb '(2))))))

(princ
 (json-encode
  `((loaded . ,(h-vec (reverse h-loaded)))
    (used . ,(h-vec (reverse h-used)))
    (calls . ,(h-vec (reverse h-calls)))
    (forge_owned_accounts . ,(if (boundp 'forge-owned-accounts)
                                 (h-vec forge-owned-accounts)
                               :json-false))
    (lookups . ,h-lookups)
    (menu_bar . ,(if menu-bar-mode t :json-false))
    (typst_watch_options . ,(h-vec (h-themed 'typst-ts-watch-options)))
    (clangd . ,(h-vec (seq-filter
                       #'stringp
                       (cdr (assoc '(c-mode c-ts-mode c++-mode c++-ts-mode
                                     objc-mode)
                                   eglot-server-programs)))))
    (chatgpt_keys . ,(h-vec (list chatgpt-shell-anthropic-key
                                  chatgpt-shell-openai-key)))
    (tmux_capabilities . ,(h-vec xterm-tmux-extra-capabilities))
    (backspace_keys . ,(h-vec (mapcar (lambda (sequence)
                                        (let ((key (lookup-key xterm-function-map
                                                               sequence)))
                                          (if (vectorp key)
                                              (key-description key)
                                            "undecoded")))
                                      '("\e[27;5;127~" "\e[27;2;127~"))))
    (keys . ,(mapcar (lambda (k) (cons k (h-key k)))
                     (split-string (getenv "DOTFILES_EMACS_KEYS") "|")))
    (region . ,(h-face 'region '(:background)))
    (match . ,(h-face 'match '(:background :foreground)))
    (region_background . ,(face-attribute 'region :background nil))
    (default . ,(mapcar (lambda (window-system)
                          (cons (or window-system 'tty)
                                (h-default-colors window-system)))
                        '(nil ns x pgtk w32)))
    (fader_end . ,(h-fader-end))
    (fader_explicit_end . ,(h-fader-end "#ff0000"))
    (frame_background_mode . ,(symbol-name frame-background-mode))
    (background_mode . ,(symbol-name (frame-parameter nil 'background-mode))))))
"""

HARNESS = HARNESS_SETUP + HARNESS_REPORT

type Report = dict[str, object]


def run_session(scenario: str, home: Path, extra_lisp: str = "") -> Report:
    emacs = shutil.which("emacs")
    assert emacs, "emacs is required"
    harness = home / "harness.el"
    _ = harness.write_text(HARNESS + extra_lisp, encoding="utf-8")
    keys = [*TERMINAL_KEYS, *GRAPHICAL_KEYS, *OTHER_KEYS]
    env = {
        "PATH": os.environ["PATH"],
        "HOME": str(home),
        "DOTFILES_EMACS_DIR": str(EMACS_DIR),
        "DOTFILES_EMACS_SCENARIO": scenario,
        "DOTFILES_EMACS_FILES": " ".join(FILES),
        "DOTFILES_EMACS_KEYS": "|".join(keys),
    }
    result = subprocess.run(
        [emacs, "--batch", "-Q", "-l", str(harness)],
        env=env,
        capture_output=True,
        check=False,
        timeout=60,
    )
    stderr = result.stderr.decode(errors="replace")
    assert result.returncode == 0, stderr
    # use-package reports a failure in a form it ran as a message, not an error.
    assert "(use-package)" not in stderr, stderr
    return cast("Report", json.loads(result.stdout))


@pytest.fixture(scope="module")
def reports(tmp_path_factory: pytest.TempPathFactory) -> dict[str, Report]:
    return {
        scenario: run_session(scenario, tmp_path_factory.mktemp(scenario))
        for scenario in ("linux", "macos")
    }


@pytest.fixture(params=["linux", "macos"])
def report(request: pytest.FixtureRequest, reports: dict[str, Report]) -> Report:
    return reports[cast("str", request.param)]


def names(report: Report, field: str) -> list[str]:
    return cast("list[str]", report[field])


def test_init_files_load(report: Report) -> None:
    assert report["loaded"] == FILES


def test_init_requires_every_file() -> None:
    init = (EMACS_DIR / "init.el").read_text(encoding="utf-8")
    required = re.findall(r"^\(require '(init-[\w-]+)\)", init, flags=re.MULTILINE)
    assert set(required) == set(FILES)


def test_macos_only_packages_are_gated(reports: dict[str, Report]) -> None:
    linux = set(names(reports["linux"], "used"))
    macos = set(names(reports["macos"], "used"))
    assert linux < macos
    assert macos - linux == GATED_PACKAGES


def test_atomic_chrome_serves_only_the_mac(reports: dict[str, Report]) -> None:
    assert "atomic-chrome-start-server" not in names(reports["linux"], "calls")
    assert "atomic-chrome-start-server" in names(reports["macos"], "calls")


def test_forge_owned_account_needs_sqlite(reports: dict[str, Report]) -> None:
    linux = reports["linux"]
    assert linux["forge_owned_accounts"] is False
    assert linux["lookups"] == 0
    macos = reports["macos"]
    assert macos["forge_owned_accounts"] == ["octocat"]
    assert macos["lookups"] == 1


def test_window_system_functions_are_guarded(reports: dict[str, Report]) -> None:
    graphical = {"tool-bar-mode", "set-fringe-mode", "set-fontset-font"}
    assert not graphical & set(names(reports["linux"], "calls"))
    assert graphical <= set(names(reports["macos"], "calls"))


def test_menu_bar_is_off(report: Report) -> None:
    assert report["menu_bar"] is False


def test_typst_opens_a_viewer_only_on_the_mac(reports: dict[str, Report]) -> None:
    assert reports["linux"]["typst_watch_options"] == []
    assert reports["macos"]["typst_watch_options"] == ["--open"]


def test_clangd_is_brews_only_on_the_mac(reports: dict[str, Report]) -> None:
    assert reports["linux"]["clangd"] == ["clangd"]
    assert reports["macos"]["clangd"] == ["/opt/homebrew/opt/llvm/bin/clangd"]


def test_chatgpt_keys_are_set_only_when_authinfo_has_them(report: Report) -> None:
    assert report["chatgpt_keys"] == ["sk-ant", "unset"]


def test_tmux_capabilities(report: Report) -> None:
    assert report["tmux_capabilities"] == ["modifyOtherKeys", "setSelection"]


def test_backspace_keys_that_tmux_extends_are_decoded(report: Report) -> None:
    assert report["backspace_keys"] == ["C-<backspace>", "S-<backspace>"]


@pytest.mark.parametrize(("key", "command"), TERMINAL_KEYS.items())
def test_terminal_keys(report: Report, key: str, command: str) -> None:
    assert cast("Mapping[str, str]", report["keys"])[key] == command


@pytest.mark.parametrize(("key", "twin"), GRAPHICAL_KEYS.items())
def test_graphical_keys_do_what_their_terminal_twins_do(
    report: Report, key: str, twin: str
) -> None:
    keys = cast("Mapping[str, str]", report["keys"])
    assert keys[key] == keys[twin]


@pytest.mark.parametrize(("key", "command"), OTHER_KEYS.items())
def test_existing_keys_are_kept(report: Report, key: str, command: str) -> None:
    assert cast("Mapping[str, str]", report["keys"])[key] == command


def test_region_and_match_have_a_color_for_every_frame(report: Report) -> None:
    region = cast("Mapping[str, Mapping[str, str]]", report["region"])
    assert region["ns"] == {"background": "selectedTextBackgroundColor"}
    assert region["other"] == {"background": "#dfecff"}
    assert region["here"] == region["other"]
    assert report["region_background"] == "#dfecff"
    match = cast("Mapping[str, Mapping[str, str]]", report["match"])
    assert match["ns"] == {"background": "findHighlightColor", "foreground": "black"}
    assert match["other"] == {"background": "yellow", "foreground": "black"}
    assert match["here"] == match["other"]


def test_a_terminal_frame_leaves_its_colors_to_the_terminal(report: Report) -> None:
    default = cast("Mapping[str, Mapping[str, str | None]]", report["default"])
    assert default["tty"] == {"background": None, "foreground": None}


@pytest.mark.parametrize("window_system", ["ns", "x", "pgtk", "w32"])
def test_a_graphical_frame_is_black_on_white(
    report: Report, window_system: str
) -> None:
    default = cast("Mapping[str, Mapping[str, str | None]]", report["default"])
    assert default[window_system] == {"background": "White", "foreground": "Black"}


def test_chatgpt_shell_fades_to_white_in_a_terminal(report: Report) -> None:
    assert report["fader_end"] == "#ffffff"


def test_chatgpt_shell_fades_to_an_explicit_background(report: Report) -> None:
    assert report["fader_explicit_end"] == "#ff0000"


def test_the_background_is_light(report: Report) -> None:
    assert report["frame_background_mode"] == "light"
    assert report["background_mode"] == "light"


WHITESPACE_TESTS = r"""
(require 'ert)
(require 'go-ts-mode)
(require 'make-mode)

(setq text-mode-hook nil
      prog-mode-hook nil
      go-ts-mode-hook nil
      go-mod-ts-mode-hook nil
      go-work-ts-mode-hook nil)

(defmacro h-with-whitespace-buffer (&rest body)
  `(let ((noninteractive nil))
     (with-temp-buffer ,@body)))

(defmacro h-with-visited-file (name text &rest body)
  "Run BODY in a buffer visiting a new file called NAME that holds TEXT."
  (declare (indent 2))
  `(let* ((noninteractive nil)
          (dir (make-temp-file "h-whitespace" t))
          (file (expand-file-name ,name dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (with-current-buffer (find-file-noselect file)
             (unwind-protect (progn ,@body)
               (set-buffer-modified-p nil)
               (kill-buffer))))
       (delete-directory dir t))))

(defun h-trailing-markers ()
  (sort (seq-filter (lambda (overlay)
                      (overlay-get overlay 'shields/trailing-space))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun h-marker-starts ()
  (mapcar #'overlay-start (h-trailing-markers)))

(defun h-marker-displays ()
  (mapcar (lambda (overlay) (overlay-get overlay 'display))
          (h-trailing-markers)))

(defun h-glyph (table char)
  "Return (CHAR . FACE) for the glyph that display TABLE shows for CHAR."
  (let ((entry (aref table char)))
    (and entry (cons (glyph-char (aref entry 0)) (glyph-face (aref entry 0))))))

(defconst h-nobreak-glyph '(?⍽ . shields/nobreak-marker))

(defun h-theme-colors (face cells &rest attributes)
  "Return ATTRIBUTES of FACE's theme spec on a display with CELLS colors."
  (cl-letf (((symbol-function 'display-color-cells) (lambda (&rest _) cells)))
    (let ((spec (face-spec-choose (cadr (assq 'shields (get face 'theme-face))))))
      (mapcar (lambda (attribute) (plist-get spec attribute)) attributes))))

(defun h-markers-are-single-spaces-p ()
  (seq-every-p (lambda (overlay)
                 (and (= (overlay-end overlay) (1+ (overlay-start overlay)))
                      (eq (char-after (overlay-start overlay)) ?\s)))
               (h-trailing-markers)))

(defun h-move (position)
  (goto-char position)
  (run-hooks 'post-command-hook))

(defun h-park ()
  (h-move (point-max)))

(defun h-has-tab-style-p ()
  (and (memq 'tabs whitespace-active-style)
       (memq 'tab-mark whitespace-active-style)
       t))

(defun h-has-standard-highlights-p ()
  (and (memq 'face whitespace-active-style)
       (memq 'space-before-tab whitespace-active-style)
       (memq 'space-after-tab whitespace-active-style)
       (memq 'missing-newline-at-eof whitespace-active-style)
       (not (memq 'trailing whitespace-active-style))
       t))

(defvar h-buffer-list-count 0)

(defun h-count-buffer-list (&rest _)
  (cl-incf h-buffer-list-count))

(defmacro h-buffer-list-calls (&rest body)
  "Return how many times BODY calls `buffer-list'."
  `(progn
     (setq h-buffer-list-count 0)
     (advice-add 'buffer-list :before #'h-count-buffer-list)
     (unwind-protect (progn ,@body)
       (advice-remove 'buffer-list #'h-count-buffer-list))
     h-buffer-list-count))

(defun h-fontify ()
  (when jit-lock-mode (jit-lock-fontify-now)))

(defun h-edit-first-line ()
  (goto-char 3)
  (insert "x")
  (run-hooks 'post-command-hook)
  (h-fontify)
  (delete-char -1)
  (run-hooks 'post-command-hook)
  (h-fontify))

(defun h-tab-marker ()
  (let ((mapping (and buffer-display-table (aref buffer-display-table ?\t))))
    (and mapping (glyph-char (aref mapping 0)))))

(ert-deftest h-whitespace-activates-automatically ()
  (h-with-whitespace-buffer
    (rename-buffer "whitespace-test" t)
    (text-mode)
    (should whitespace-mode)
    (insert "value\t \n")
    (jit-lock-fontify-now)
    (should (eq (h-tab-marker) ?⇥))
    (should (= (length (h-trailing-markers)) 1))
    (makefile-mode)
    (should whitespace-mode)
    (jit-lock-fontify-now)
    (should-not (h-tab-marker))
    (should (= (length (h-trailing-markers)) 1))))

(ert-deftest h-whitespace-markers-preserve-text-and-tab-stops ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a\tb \t \nnext\n")
    (whitespace-mode 1)
    (set-buffer-modified-p nil)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(4 6)))
    (dolist (overlay (h-trailing-markers))
      (should (equal (overlay-get overlay 'display) "␣"))
      (should (= (- (overlay-end overlay) (overlay-start overlay)) 1)))
    (should (eq (h-tab-marker) ?⇥))
    (should (eq (aref (aref buffer-display-table ?\t) 1) ?\t))
    (should (equal (buffer-string) "a\tb \t \nnext\n"))
    (should-not (buffer-modified-p))
    (should (eq (glyph-char (display-table-slot buffer-display-table 'wrap)) ?⸗))
    (should-not truncate-lines)
    (h-move 1)
    (end-of-line)
    (insert "x")
    (run-hooks 'post-command-hook)
    (jit-lock-fontify-now)
    (should-not (h-trailing-markers))
    (delete-char -1)
    (run-hooks 'post-command-hook)
    (jit-lock-fontify-now)
    (should-not (h-trailing-markers))
    (h-park)
    (should (= (length (h-trailing-markers)) 2))
    (whitespace-mode -1)
    (should-not (h-trailing-markers))))

(ert-deftest h-whitespace-markers-ignore-field-boundaries ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "abcd   \n")
    (put-text-property 1 5 'field 'prompt)
    (whitespace-mode 1)
    (jit-lock-fontify-now 2 3)
    (should (equal (h-marker-starts) '(5 6 7)))))

(ert-deftest h-whitespace-keeps-the-stock-space-and-newline-marks ()
  (h-with-whitespace-buffer
    (text-mode)
    (setq-local whitespace-style '(face tab-mark space-mark newline-mark))
    (whitespace-mode 1)
    (should (eq (h-tab-marker) ?⇥))
    (should (equal (aref buffer-display-table ?\s) [?·]))
    (should (equal (aref buffer-display-table ?\n) [?$ ?\n]))))

(ert-deftest h-whitespace-tab-required-modes-exist ()
  (dolist (mode shields/tab-required-modes)
    (should (fboundp mode))))

(ert-deftest h-whitespace-tab-required-formats-use-their-real-modes ()
  (dolist (case '(("main.go" go-ts-mode)
                  ("go.mod" go-mod-ts-mode)
                  ("go.work" go-work-ts-mode)
                  ("Makefile" makefile-mode)
                  ("makefile" makefile-mode)
                  ("GNUmakefile" makefile-mode)
                  ("BSDmakefile" makefile-mode)
                  ("rules.mk" makefile-mode)
                  ("Makefile.am" makefile-mode)))
    (h-with-visited-file (car case) "\tvalue \t \n"
      (should (derived-mode-p (cadr case)))
      (should whitespace-mode)
      (jit-lock-fontify-now)
      (should-not (h-tab-marker))
      (should-not (h-has-tab-style-p))
      (should (h-has-standard-highlights-p))
      (should (= (length (h-trailing-markers)) 2)))))

(ert-deftest h-whitespace-makefile-looking-names-follow-the-major-mode ()
  (dolist (name '("Makefile.common" "Makefile.local" "Makefile.linux"
                  "rules.mak"))
    (h-with-visited-file name "\tvalue \t \n"
      (should (eq major-mode 'fundamental-mode))
      (should whitespace-mode)
      (jit-lock-fontify-now)
      (should (eq (h-tab-marker) ?⇥))
      (should (h-has-tab-style-p))
      (should (= (length (h-trailing-markers)) 2)))))

(ert-deftest h-whitespace-other-formats-show-tabs ()
  (dolist (name '("notes.txt" "data.tsv"))
    (h-with-visited-file name "one\ttwo \n"
      (should whitespace-mode)
      (jit-lock-fontify-now)
      (should (eq (h-tab-marker) ?⇥))
      (should (h-has-tab-style-p))
      (should (h-has-standard-highlights-p))
      (should (= (length (h-trailing-markers)) 1)))))

(ert-deftest h-whitespace-highlights-misplaced-spaces-and-missing-newlines ()
  (h-with-visited-file "Makefile" "  \ta\n\t        b\nc"
    (font-lock-ensure)
    (should (eq (get-text-property 1 'face) 'whitespace-space-before-tab))
    (should (eq (get-text-property 2 'face) 'whitespace-space-before-tab))
    (should-not (get-text-property 3 'face))
    (should (eq (get-text-property 7 'face) 'whitespace-space-after-tab))
    (should (eq (get-text-property 14 'face) 'whitespace-space-after-tab))
    (should-not (get-text-property 6 'face))
    (should (memq 'whitespace-missing-newline-at-eof
                  (ensure-list (get-text-property 17 'face)))))
  (h-with-whitespace-buffer
    (rename-buffer "h-misplaced" t)
    (text-mode)
    (insert "x \ty\n\t        z\n")
    (font-lock-ensure)
    (should (eq (get-text-property 3 'face) 'whitespace-space-before-tab))
    (should (eq (get-text-property 6 'face) 'whitespace-space-after-tab))
    (should-not (get-text-property 2 'face))
    (should (eq (h-tab-marker) ?⇥))))

(ert-deftest h-whitespace-misplaced-spaces-keep-the-trailing-markers ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "x \t \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (font-lock-ensure)
    (should (equal (h-marker-starts) '(2 4)))
    (should (eq (h-tab-marker) ?⇥))
    (should (eq (get-text-property 3 'face) 'whitespace-space-before-tab))
    (makefile-mode)
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (font-lock-ensure)
    (should (equal (h-marker-starts) '(2 4)))
    (should-not (h-tab-marker))
    (should (eq (get-text-property 2 'face) 'whitespace-space-before-tab))
    (should-not (get-text-property 3 'face))
    (dolist (overlay (h-trailing-markers))
      (should (equal (overlay-get overlay 'display) "␣"))
      (should (eq (overlay-get overlay 'face) 'shields/whitespace)))))

(ert-deftest h-whitespace-leaves-the-blanks-at-point-unmarked ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "one \ntwo  \nthree\n")
    (whitespace-mode 1)
    (h-move 5)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(9 10)))
    (h-move 11)
    (should (equal (h-marker-starts) '(4)))
    (h-move 10)
    (should (equal (h-marker-starts) '(4 9 10)))
    (h-move 11)
    (should (equal (h-marker-starts) '(4)))
    (h-move 17)
    (should (equal (h-marker-starts) '(4 9 10)))
    (h-move 5)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(9 10)))
    (whitespace-mode -1)
    (should-not (h-trailing-markers))
    (should-not (memq #'shields/follow-point post-command-hook))))

(ert-deftest h-whitespace-exemption-survives-other-fontification-functions ()
  (h-with-whitespace-buffer
    (rename-buffer "h-font-lock" t)
    (text-mode)
    (font-lock-mode 1)
    (insert "one \ntwo  \n")
    (whitespace-mode 1)
    (h-move 5)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(9 10)))
    (goto-char 5)
    (insert " ")
    (run-hooks 'post-command-hook)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(10 11)))))

(ert-deftest h-whitespace-leaves-fresh-prompts-unmarked ()
  (h-with-whitespace-buffer
    (comint-mode)
    (whitespace-mode 1)
    (insert "output \n$ ")
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(7)))
    (insert "ls ")
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(7)))
    (h-move 1)
    (should (equal (h-marker-starts) '(7 13)))
    (h-park)
    (should (equal (h-marker-starts) '(7)))
    (insert "-l")
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(7)))))

(ert-deftest h-whitespace-follows-point-through-commands ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "alpha \nbeta  \n")
    (whitespace-mode 1)
    (goto-char (point-min))
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (jit-lock-fontify-now)
      (should (equal (h-marker-starts) '(6 12 13)))
      (execute-kbd-macro (kbd "C-e"))
      (should (equal (h-marker-starts) '(12 13)))
      (execute-kbd-macro (kbd "C-n C-e"))
      (should (equal (h-marker-starts) '(6)))
      (execute-kbd-macro (kbd "M->"))
      (execute-kbd-macro "word ")
      (jit-lock-fontify-now)
      (should (equal (h-marker-starts) '(6 12 13)))
      (execute-kbd-macro "x")
      (jit-lock-fontify-now)
      (should (equal (h-marker-starts) '(6 12 13)))
      (execute-kbd-macro (kbd "DEL"))
      (jit-lock-fontify-now)
      (should (equal (h-marker-starts) '(6 12 13)))
      (execute-kbd-macro (kbd "C-p"))
      (jit-lock-fontify-now)
      (should (equal (h-marker-starts) '(6 12 13 19))))))

(ert-deftest h-whitespace-text-typed-before-blanks-stays-before-them ()
  (dolist (case '(("foo  \n" 4 "xyz" "fooxyz  \n" 7)
                  ("    \n" 1 "ab" "ab    \n" 3)))
    (h-with-whitespace-buffer
      (text-mode)
      (insert (nth 0 case))
      (whitespace-mode 1)
      (goto-char (nth 1 case))
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (jit-lock-fontify-now)
        (execute-kbd-macro (nth 2 case))
        (should (equal (buffer-string) (nth 3 case)))
        (should (= (point) (nth 4 case)))))))

(ert-deftest h-whitespace-text-inserted-at-a-marked-space-is-not-marked ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a  \nb\n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (goto-char 2)
    (insert "x")
    (should (h-markers-are-single-spaces-p))
    (should (equal (h-marker-starts) '(3 4)))
    (goto-char 4)
    (insert "y")
    (should (h-markers-are-single-spaces-p))
    (should (equal (h-marker-starts) '(3 5)))))

(ert-deftest h-whitespace-large-insertion-at-a-marked-space-leaves-no-stale-marker ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "foo  \nbar\n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (goto-char 4)
    (insert (apply #'concat (make-list jit-lock-chunk-size "row\n")))
    (jit-lock-function (point-min))
    (should (h-markers-are-single-spaces-p))
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts)
                   (list (+ 4 (* 4 jit-lock-chunk-size))
                         (+ 5 (* 4 jit-lock-chunk-size)))))))

(ert-deftest h-whitespace-buffers-sharing-text-follow-their-own-points ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \nb \n")
    (whitespace-mode 1)
    (h-move 3)
    (let ((clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (progn
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(5)))
            (with-current-buffer clone
              (should (equal (h-marker-starts) '(5)))
              (h-move 6)
              (should (equal (h-marker-starts) '(2)))
              (h-move 1)
              (should (equal (h-marker-starts) '(2 5))))
            (should (equal (h-marker-starts) '(5)))
            (h-move 6)
            (should (equal (h-marker-starts) '(2)))
            (with-current-buffer clone
              (should (equal (h-marker-starts) '(2 5)))))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-siblings-keep-point-when-text-is-inserted-there ()
  (dolist (case '(("alpha\nlast\n" 6 " " (6) nil)
                  ("one \ntwo\n" 5 "\n" nil (4))))
    (cl-destructuring-bind (text position insertion markers clone-markers) case
      (h-with-whitespace-buffer
        (text-mode)
        (insert text)
        (whitespace-mode 1)
        (h-move position)
        (jit-lock-fontify-now)
        (let ((clone (clone-indirect-buffer nil nil)))
          (unwind-protect
              (progn
                (with-current-buffer clone
                  (insert insertion)
                  (should (equal (h-marker-starts) clone-markers)))
                (should (= (point) position))
                (should (equal (h-marker-starts) markers)))
            (kill-buffer clone)))))))

(ert-deftest h-whitespace-clones-follow-point-after-a-sibling-inserts-there ()
  (dolist (case '(("alpha\nlast\n" 6 " " (6))
                  ("one \ntwo\n" 5 "\n" nil)))
    (cl-destructuring-bind (text position insertion markers) case
      (h-with-whitespace-buffer
        (text-mode)
        (insert text)
        (whitespace-mode 1)
        (h-move position)
        (jit-lock-fontify-now)
        (let ((sibling (clone-indirect-buffer nil nil)))
          (unwind-protect
              (progn
                (with-current-buffer sibling
                  (insert insertion))
                (let ((clone (clone-indirect-buffer nil nil)))
                  (unwind-protect
                      (with-current-buffer clone
                        (should (= (point) position))
                        (should (equal (h-marker-starts) markers))
                        (h-park)
                        (h-move position)
                        (should (equal (h-marker-starts) markers)))
                    (kill-buffer clone))))
            (kill-buffer sibling)))))))

(ert-deftest h-whitespace-indirect-buffers-share-marker-updates ()
  (dolist (fontify-before-cloning '(nil t))
    (h-with-whitespace-buffer
      (text-mode)
      (insert "a \nnext \n")
      (whitespace-mode 1)
      (when fontify-before-cloning (jit-lock-fontify-now))
      (let* ((base (current-buffer))
             (clone (clone-indirect-buffer nil nil)))
        (unwind-protect
            (progn
              (dolist (buffer (list base clone))
                (with-current-buffer buffer
                  (jit-lock-fontify-now)
                  (should (equal (h-marker-starts) '(2 8)))))
              (dolist (editing-buffer (list base clone))
                (with-current-buffer editing-buffer
                  (goto-char 3)
                  (insert "x")
                  (jit-lock-fontify-now))
                (dolist (buffer (list base clone))
                  (with-current-buffer buffer
                    (jit-lock-fontify-now)
                    (should (equal (h-marker-starts) '(9)))))
                (with-current-buffer editing-buffer
                  (delete-char -1)
                  (h-park)
                  (jit-lock-fontify-now))
                (dolist (buffer (list clone base))
                  (with-current-buffer buffer
                    (jit-lock-fontify-now)
                    (should (equal (h-marker-starts) '(2 8)))))))
          (kill-buffer clone))))))

(ert-deftest h-whitespace-plain-indirect-buffers-share-marker-updates ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \nnext\n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (let* ((base (current-buffer))
           (indirect (make-indirect-buffer base "h-plain-indirect")))
      (unwind-protect
          (progn
            (with-current-buffer indirect
              (text-mode)
              (should whitespace-mode)
              (goto-char 8)
              (insert " ")
              (h-park))
            (dolist (buffer (list base indirect))
              (with-current-buffer buffer
                (should (equal (h-marker-starts) '(2 8)))))
            (goto-char 3)
            (insert "b")
            (dolist (buffer (list indirect base))
              (with-current-buffer buffer
                (should (equal (h-marker-starts) '(9))))))
        (kill-buffer indirect)))))

(ert-deftest h-whitespace-indirect-buffers-keep-local-display-state ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \nnext \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (let* ((base (current-buffer))
           (clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (narrow-to-region 4 (point-max)))
            (whitespace-mode -1)
            (goto-char 3)
            (insert "x")
            (with-current-buffer clone
              (h-fontify)
              (should (= (point-min) 5))
              (widen)
              (h-fontify)
              (should (equal (h-marker-starts) '(9))))
            (should-not (h-trailing-markers))
            (with-current-buffer clone
              (whitespace-mode -1))
            (whitespace-mode 1)
            (jit-lock-fontify-now)
            (with-current-buffer clone
              (goto-char 4)
              (delete-char -1)
              (should-not (h-trailing-markers)))
            (h-park)
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(2 8)))
            (with-current-buffer clone
              (whitespace-mode 1)
              (h-fontify))
            (let ((global-whitespace-mode nil))
              (text-mode))
            (goto-char 3)
            (insert "x")
            (with-current-buffer clone
              (h-fontify)
              (should (equal (h-marker-starts) '(9))))
            (should-not (h-trailing-markers))
            (should (eq (current-buffer) base)))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-direct-clones-keep-independent-markers ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (let ((clone (clone-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (jit-lock-fontify-now)
              (should (equal (h-marker-starts) '(2))))
            (goto-char 3)
            (insert "x")
            (jit-lock-fontify-now)
            (should-not (h-trailing-markers))
            (with-current-buffer clone
              (should (equal (buffer-string) "a \n"))
              (should (equal (h-marker-starts) '(2)))))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-tsv-and-mode-changes ()
  (h-with-whitespace-buffer
    (setq buffer-file-name "/tmp/example.tsv")
    (text-mode)
    (insert "one\ttwo \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (should (eq (h-tab-marker) ?⇥))
    (should (h-has-tab-style-p))
    (should (h-has-standard-highlights-p))
    (should (= (length (h-trailing-markers)) 1))
    (makefile-mode)
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (should-not (h-tab-marker))
    (should-not (h-has-tab-style-p))
    (should (h-has-standard-highlights-p))
    (should (= (length (h-trailing-markers)) 1))
    (text-mode)
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (should (eq (h-tab-marker) ?⇥))
    (should (h-has-tab-style-p))
    (setq-local shields/show-tabs nil)
    (run-hooks 'hack-local-variables-hook)
    (jit-lock-fontify-now)
    (should-not (h-tab-marker))
    (should-not (h-has-tab-style-p))
    (should (h-has-standard-highlights-p))
    (should (= (length (h-trailing-markers)) 1))
    (setq-local shields/show-tabs t)
    (run-hooks 'hack-local-variables-hook)
    (should (h-has-tab-style-p))
    (should (h-has-standard-highlights-p))
    (narrow-to-region 1 2)
    (whitespace-mode -1)
    (widen)
    (should-not (h-trailing-markers))))

(ert-deftest h-whitespace-lone-buffers-do-no-work-after-edits ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \nb \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (should-not shields/text-peers)
    (should-not (memq #'shields/sync-trailing-space-markers
                      after-change-functions))
    (should (= (h-buffer-list-calls (h-edit-first-line)) 0))
    (h-park)
    (should (equal (h-marker-starts) '(2 5)))))

(ert-deftest h-whitespace-siblings-update-without-scanning-buffers ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \nb \n")
    (whitespace-mode 1)
    (let ((clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (dolist (editing-buffer (list (current-buffer) clone))
            (with-current-buffer editing-buffer
              (should (memq #'shields/sync-trailing-space-markers
                            after-change-functions))
              (should (= (h-buffer-list-calls (h-edit-first-line)) 0)))
            (dolist (buffer (list (current-buffer) clone))
              (with-current-buffer buffer
                (h-park)
                (should (equal (h-marker-starts) '(2 5))))))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-last-sibling-returns-to-the-cheap-path ()
  (dolist (order '((0 1) (1 0)))
    (h-with-whitespace-buffer
      (text-mode)
      (insert "a \nb \n")
      (whitespace-mode 1)
      (jit-lock-fontify-now)
      (let* ((base (current-buffer))
             (c1 (clone-indirect-buffer nil nil))
             (c2 (with-current-buffer c1 (clone-indirect-buffer nil nil)))
             (clones (list c1 c2))
             (leaving (nth (car order) clones))
             (staying (nth (cadr order) clones)))
        (dolist (buffer (list base c1 c2))
          (with-current-buffer buffer
            (should (= (length shields/text-peers) 3))
            (should (memq #'shields/sync-trailing-space-markers
                          after-change-functions))))
        (kill-buffer leaving)
        (dolist (buffer (list base staying))
          (with-current-buffer buffer
            (should (= (length shields/text-peers) 2))
            (should (memq #'shields/sync-trailing-space-markers
                          after-change-functions))))
        (with-current-buffer staying
          (goto-char 3)
          (insert "x")
          (h-park))
        (h-park)
        (should (equal (h-marker-starts) '(6)))
        (kill-buffer staying)
        (should-not shields/text-peers)
        (should-not (memq #'shields/sync-trailing-space-markers
                          after-change-functions))
        (should-not (memq #'shields/leave-text kill-buffer-hook))
        (should (= (h-buffer-list-calls
                     (goto-char 4)
                     (delete-char -1)
                     (run-hooks 'post-command-hook)
                     (jit-lock-fontify-now))
                   0))
        (h-park)
        (should (equal (h-marker-starts) '(2 5)))))))

(ert-deftest h-whitespace-plain-indirect-buffers-without-a-mode-update-siblings ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a x\nb \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(6)))
    (let ((plain (make-indirect-buffer (current-buffer) "h-plain-fundamental")))
      (unwind-protect
          (progn
            (with-current-buffer plain
              (should (eq major-mode 'fundamental-mode))
              (should-not whitespace-mode)
              (goto-char 4)
              (delete-char -1)
              (goto-char (point-max)))
            (h-park)
            (should (equal (h-marker-starts) '(2 5)))
            (with-current-buffer plain
              (should-not (h-trailing-markers))
              (whitespace-mode 1)
              (should (equal (h-marker-starts) '(2 5)))
              (goto-char 3)
              (insert "y")
              (h-park))
            (h-park)
            (should (equal (h-marker-starts) '(6)))
            (with-current-buffer plain
              (should (equal (h-marker-starts) '(6)))
              (whitespace-mode -1)
              (should-not (h-trailing-markers))
              (goto-char 4)
              (delete-char -1))
            (h-park)
            (should (equal (h-marker-starts) '(2 5))))
          (kill-buffer plain))
      (should-not shields/text-peers)
      (should-not (memq #'shields/sync-trailing-space-markers
                        after-change-functions)))))

(ert-deftest h-whitespace-siblings-follow-toggles-and-mode-changes ()
  (h-with-whitespace-buffer
    (rename-buffer "h-toggles" t)
    (text-mode)
    (insert "a x\nb \n")
    (whitespace-mode 1)
    (let ((base (current-buffer))
          (clone (clone-indirect-buffer "h-toggles-clone" nil))
          (with-x '(6))
          (without-x '(2 5)))
      (unwind-protect
          (cl-flet ((flip-x (buffer)
                      (with-current-buffer buffer
                        (goto-char 3)
                        (if (eq (char-after) ?x)
                            (delete-char 1)
                          (insert "x"))
                        (h-park)))
                    (markers-in (buffer)
                      (with-current-buffer buffer
                        (h-fontify)
                        (h-marker-starts))))
            (should (equal (markers-in base) with-x))
            (should (equal (markers-in clone) with-x))
            (with-current-buffer clone
              (whitespace-mode -1))
            (flip-x clone)
            (should (equal (markers-in base) without-x))
            (should-not (markers-in clone))
            (flip-x clone)
            (should (equal (markers-in base) with-x))
            (with-current-buffer clone
              (let ((whitespace-global-modes '(not makefile-mode)))
                (makefile-mode)))
            (should-not (buffer-local-value 'whitespace-mode clone))
            (flip-x clone)
            (should (equal (markers-in base) without-x))
            (flip-x clone)
            (with-current-buffer clone
              (whitespace-mode 1))
            (should (equal (markers-in clone) with-x))
            (flip-x clone)
            (should (equal (markers-in base) without-x))
            (should (equal (markers-in clone) without-x))
            (with-current-buffer base
              (whitespace-mode -1))
            (flip-x clone)
            (should-not (markers-in base))
            (should (equal (markers-in clone) with-x))
            (with-current-buffer base
              (whitespace-mode 1))
            (should (equal (markers-in base) with-x))
            (flip-x base)
            (should (equal (markers-in base) without-x))
            (should (equal (markers-in clone) without-x))
            (with-current-buffer clone
              (text-mode))
            (flip-x base)
            (should (equal (markers-in base) with-x))
            (should (equal (markers-in clone) with-x))
            (with-current-buffer clone
              (whitespace-mode -1))
            (with-current-buffer base
              (whitespace-mode -1))
            (dolist (buffer (list base clone))
              (with-current-buffer buffer
                (should-not shields/text-peers)
                (should-not (memq #'shields/sync-trailing-space-markers
                                  after-change-functions)))))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-direct-clones-of-shared-buffers-stand-alone ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (let* ((indirect (clone-indirect-buffer nil nil))
           (direct (clone-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer direct
              (should-not shields/text-peers)
              (should-not (memq #'shields/sync-trailing-space-markers
                                after-change-functions))
              (goto-char 3)
              (insert "x")
              (jit-lock-fontify-now))
            (should (equal (buffer-string) "a \n"))
            (should (equal (h-marker-starts) '(2)))
            (with-current-buffer indirect
              (should (equal (h-marker-starts) '(2)))))
        (kill-buffer direct)
        (kill-buffer indirect)))))

(ert-deftest h-whitespace-base-keeps-markers-a-clone-fontified-first ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \n" (make-string 3000 ?x) "\nb  \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now 1 100)
    (let ((clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (jit-lock-fontify-now))
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(2 3006 3007)))
            (kill-buffer clone)
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(2 3006 3007))))
        (when (buffer-live-p clone)
          (kill-buffer clone))))))

(ert-deftest h-whitespace-base-keeps-markers-after-its-own-mode-change ()
  (h-with-whitespace-buffer
    (rename-buffer "h-base-mode" t)
    (text-mode)
    (insert "a \n" (make-string 3000 ?x) "\nb  \n")
    (whitespace-mode 1)
    (let ((clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (progn
            (text-mode)
            (should whitespace-mode)
            (with-current-buffer clone
              (h-fontify))
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(2 3006 3007))))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-base-keeps-markers-when-it-joins-siblings ()
  (h-with-whitespace-buffer
    (rename-buffer "h-base-toggle" t)
    (text-mode)
    (font-lock-mode 1)
    (insert "a \n" (make-string 3000 ?x) "\nb  \n")
    (whitespace-mode -1)
    (let ((clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (whitespace-mode 1))
            (whitespace-mode 1)
            (with-current-buffer clone
              (h-fontify))
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(2 3006 3007))))
        (kill-buffer clone)))))

(ert-deftest h-whitespace-base-keeps-markers-a-plain-indirect-buffer-fontified ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \n" (make-string 3000 ?x) "\nb  \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now 1 100)
    (let ((plain (make-indirect-buffer (current-buffer) "h-plain-fontified")))
      (unwind-protect
          (progn
            (with-current-buffer plain
              (jit-lock-register #'ignore)
              (jit-lock-fontify-now))
            (jit-lock-fontify-now)
            (should (equal (h-marker-starts) '(2 3006 3007))))
        (kill-buffer plain)))))

(ert-deftest h-whitespace-keeps-a-style-set-for-the-buffer ()
  (let* ((noninteractive nil)
         (enable-local-variables :all)
         (dir (make-temp-file "h-whitespace" t))
         (subdir (expand-file-name "sub" dir)))
    (unwind-protect
        (progn
          (make-directory subdir)
          (with-temp-file (expand-file-name ".dir-locals.el" subdir)
            (insert "((nil . ((whitespace-style . (face trailing empty)))))"))
          (with-temp-file (expand-file-name "a.txt" subdir)
            (insert "x\t \n"))
          (with-temp-file (expand-file-name "b.txt" dir)
            (insert "x\t \n\n;; Local Variables:\n"
                    ";; whitespace-style: (face empty)\n;; End:\n"))
          (with-temp-file (expand-file-name "c.txt" dir)
            (insert "x\t \n\n;; Local Variables:\n"
                    ";; whitespace-style: nil\n;; End:\n"))
          (dolist (case '(("sub/a.txt" (face trailing empty) t)
                          ("b.txt" (face empty) t)
                          ("c.txt" nil nil)))
            (with-current-buffer (find-file-noselect
                                  (expand-file-name (car case) dir))
              (unwind-protect
                  (progn
                    (should whitespace-mode)
                    (should (equal whitespace-style (cadr case)))
                    (should (equal whitespace-active-style (cadr case)))
                    (h-fontify)
                    (should (eq (and (h-marker-starts) t) (nth 2 case)))
                    (setq-local shields/show-tabs nil)
                    (run-hooks 'hack-local-variables-hook)
                    (should (equal whitespace-style (cadr case)))
                    (should (equal whitespace-active-style (cadr case))))
                (set-buffer-modified-p nil)
                (kill-buffer))))
          (h-with-whitespace-buffer
            (text-mode)
            (setq-local whitespace-style '(face empty))
            (whitespace-mode 1)
            (shields/update-whitespace-style)
            (run-hooks 'hack-local-variables-hook)
            (setq-local shields/show-tabs nil)
            (shields/update-whitespace-style)
            (should (equal whitespace-style '(face empty)))
            (makefile-mode)
            (whitespace-mode 1)
            (should (h-has-standard-highlights-p))
            (should-not (h-has-tab-style-p))))
      (delete-directory dir t))))

(ert-deftest h-whitespace-empty-style-shows-no-markers ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \nb\n")
    (setq-local whitespace-style nil)
    (whitespace-mode 1)
    (h-fontify)
    (should-not (h-marker-starts))
    (should-not shields/point)
    (should-not (memq #'shields/follow-point post-command-hook))
    (should-not (memq #'shields/fontify-trailing-spaces jit-lock-functions))
    (kill-local-variable 'whitespace-style)
    (whitespace-mode 1)
    (h-fontify)
    (should (equal (h-marker-starts) '(2)))
    (let ((indirect (make-indirect-buffer (current-buffer) "h-bare" t)))
      (unwind-protect
          (progn
            (with-current-buffer indirect
              (setq-local whitespace-style nil)
              (whitespace-mode 1))
            (goto-char 3)
            (insert " ")
            (h-park)
            (h-fontify)
            (should (equal (h-marker-starts) '(2 3)))
            (with-current-buffer indirect
              (should-not (h-marker-starts))))
        (kill-buffer indirect)))))

(ert-deftest h-whitespace-unchanged-style-leaves-the-mode-running ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a \n")
    (whitespace-mode 1)
    (let ((toggles 0))
      (add-hook 'whitespace-mode-hook (lambda () (cl-incf toggles)) nil t)
      (shields/update-whitespace-style)
      (run-hooks 'hack-local-variables-hook)
      (should (= toggles 0))
      (setq-local shields/show-tabs nil)
      (shields/update-whitespace-style)
      (should (= toggles 2)))))

(ert-deftest h-whitespace-exemption-ends-after-narrowing-moves-point ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "one \ntwo\nthree\n")
    (whitespace-mode 1)
    (h-move 5)
    (jit-lock-fontify-now)
    (should-not (h-marker-starts))
    (narrow-to-region 9 (point-max))
    (h-move 9)
    (widen)
    (should (equal (h-marker-starts) '(4)))))

(ert-deftest h-whitespace-siblings-mark-text-outside-their-narrowing ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a\nb \nc \n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (let ((clone (clone-indirect-buffer nil nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (narrow-to-region 3 5))
            (goto-char 2)
            (insert " ")
            (h-park)
            (with-current-buffer clone
              (widen)
              (h-park)
              (should (equal (h-marker-starts) '(2 5 8)))))
        (kill-buffer clone)))))

(defvar h-overlays-made 0)

(defun h-count-overlay (&rest _)
  (cl-incf h-overlays-made))

(ert-deftest h-whitespace-jit-lock-scans-each-line-once ()
  (dolist (with-font-lock '(nil t))
    (h-with-whitespace-buffer
      (rename-buffer "h-long-line" t)
      (text-mode)
      (font-lock-mode (if with-font-lock 1 -1))
      (insert "start" (make-string (* 4 jit-lock-chunk-size) ?\s) "\nend \n")
      (whitespace-mode 1)
      (setq h-overlays-made 0)
      (advice-add 'make-overlay :before #'h-count-overlay)
      (unwind-protect
          (let ((chunks 0) position)
            (while (and (< chunks 100)
                        (setq position (text-property-any
                                        (point-min) (point-max) 'fontified nil)))
              (jit-lock-function position)
              (cl-incf chunks))
            (should (< chunks 100)))
        (advice-remove 'make-overlay #'h-count-overlay))
      (should (= h-overlays-made (1+ (* 4 jit-lock-chunk-size))))
      (should (= (length (h-marker-starts)) (1+ (* 4 jit-lock-chunk-size)))))))

(ert-deftest h-whitespace-an-edit-rescans-only-its-own-line ()
  (h-with-whitespace-buffer
    (rename-buffer "h-own-line" t)
    (text-mode)
    (font-lock-mode 1)
    (insert "a \n" (make-string 50 ?\s) "\nlast\n")
    (whitespace-mode 1)
    (jit-lock-fontify-now)
    (should (= (length (h-marker-starts)) 51))
    (setq h-overlays-made 0)
    (advice-add 'make-overlay :before #'h-count-overlay)
    (unwind-protect
        (progn
          (goto-char 2)
          (insert "x")
          (jit-lock-fontify-now)
          (delete-char -1)
          (jit-lock-fontify-now))
      (advice-remove 'make-overlay #'h-count-overlay))
    (should (= h-overlays-made 2))
    (should (= (length (h-marker-starts)) 51))))

(ert-deftest h-whitespace-a-replacement-ending-in-a-newline-leaves-no-marker ()
  (dolist (with-font-lock '(nil t))
    (h-with-whitespace-buffer
      (rename-buffer "h-replacement" t)
      (text-mode)
      (font-lock-mode (if with-font-lock 1 -1))
      (insert "a \nb\n")
      (whitespace-mode 1)
      (jit-lock-fontify-now)
      (should (equal (h-marker-starts) '(2)))
      (goto-char 2)
      (should (looking-at " "))
      (replace-match "x\n")
      (jit-lock-fontify-now)
      (should (equal (buffer-string) "ax\n\nb\n"))
      (should-not (h-marker-starts)))))

(ert-deftest h-whitespace-no-break-spaces-have-a-box-in-every-table ()
  (dolist (char '(#xA0 #x202F))
    (should (equal (h-glyph standard-display-table char) h-nobreak-glyph)))
  (dolist (own-table '(nil t))
    (h-with-whitespace-buffer
      (text-mode)
      (whitespace-mode -1)
      (when own-table
        (setq buffer-display-table (make-display-table)))
      (whitespace-mode 1)
      (should buffer-display-table)
      (dolist (char '(#xA0 #x202F))
        (should (equal (h-glyph buffer-display-table char) h-nobreak-glyph))))))

(ert-deftest h-whitespace-other-special-spaces-and-hyphens-are-left-alone ()
  (dolist (char '(#x2007 #x2010 #x2011 #xAD #x2060))
    (should-not (aref standard-display-table char)))
  (h-with-whitespace-buffer
    (text-mode)
    (whitespace-mode 1)
    (dolist (char '(#x2007 #x2010 #x2011 #xAD #x2060))
      (should-not (aref buffer-display-table char)))))

(ert-deftest h-whitespace-explicit-space-marks-still-win ()
  (h-with-whitespace-buffer
    (text-mode)
    (setq-local whitespace-style '(face space-mark))
    (whitespace-mode 1)
    (should (equal (aref buffer-display-table #xA0) [?¤]))
    (should (equal (h-glyph buffer-display-table #x202F) h-nobreak-glyph))))

(ert-deftest h-whitespace-marks-trailing-no-break-spaces ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "a\u00A0b \u00A0\t\u202F \nc\u00A0\nd\u00A0e\n")
    (whitespace-mode 1)
    (set-buffer-modified-p nil)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(4 5 7 8 11)))
    (should (equal (h-marker-displays) '("␣" "⍽" "⍽" "␣" "⍽")))
    (should (= (length (cl-remove-duplicates (h-marker-displays) :test #'eq))
               (length (h-trailing-markers))))
    (dolist (overlay (h-trailing-markers))
      (should (eq (overlay-get overlay 'face) 'shields/whitespace))
      (should (= (- (overlay-end overlay) (overlay-start overlay)) 1)))
    (should (equal (buffer-string) "a\u00A0b \u00A0\t\u202F \nc\u00A0\nd\u00A0e\n"))
    (should-not (buffer-modified-p))))

(ert-deftest h-whitespace-leaves-a-trailing-run-ending-at-point-unmarked ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "one \u00A0 \u202F\ntwo \nthree\n")
    (whitespace-mode 1)
    (h-move 8)
    (jit-lock-fontify-now)
    (should (equal (h-marker-starts) '(12)))
    (h-move 7)
    (should (equal (h-marker-starts) '(4 5 6 7 12)))
    (should (equal (h-marker-displays) '("␣" "⍽" "␣" "⍽" "␣")))
    (h-move 13)
    (should (equal (h-marker-starts) '(4 5 6 7)))
    (h-move 8)
    (should (equal (h-marker-starts) '(12)))
    (h-move 5)
    (should (equal (h-marker-starts) '(4 5 6 7 12)))
    (h-park)
    (should (equal (h-marker-starts) '(4 5 6 7 12)))
    (dolist (char '(#xA0 #x202F))
      (should (equal (h-glyph buffer-display-table char) h-nobreak-glyph)))))

(ert-deftest h-whitespace-follows-point-off-a-line-ending-in-a-no-break-space ()
  (h-with-whitespace-buffer
    (text-mode)
    (insert "ab\u00A0\nnext\n")
    (whitespace-mode 1)
    (h-move 4)
    (jit-lock-fontify-now)
    (should-not (h-marker-starts))
    (h-move 5)
    (should (equal (h-marker-starts) '(3)))
    (should (equal (h-marker-displays) '("⍽")))
    (h-move 4)
    (should-not (h-marker-starts))))

(ert-deftest h-whitespace-a-nil-style-keeps-only-the-gray-box ()
  (dolist (own-table '(nil t))
    (h-with-whitespace-buffer
      (text-mode)
      (insert "a\u00A0b \u00A0\n")
      (whitespace-mode -1)
      (when own-table
        (setq buffer-display-table (make-display-table)))
      (setq-local whitespace-style nil)
      (whitespace-mode 1)
      (h-fontify)
      (should-not (h-marker-starts))
      (should-not shields/point)
      (should (eq (and buffer-display-table t) own-table))
      (dolist (char '(#xA0 #x202F))
        (should (equal (h-glyph (or buffer-display-table standard-display-table)
                                char)
                       h-nobreak-glyph))))))

(ert-deftest h-whitespace-markers-go-in-a-copy-of-a-shared-display-table ()
  (let ((shared (make-display-table)))
    (aset shared ?x [?y])
    (dolist (style '(nil (face) (face tab-mark)))
      (h-with-whitespace-buffer
        (text-mode)
        (whitespace-mode -1)
        (setq buffer-display-table shared)
        (setq-local whitespace-style style)
        (whitespace-mode 1)
        (should-not (eq buffer-display-table shared))
        (should (equal (aref buffer-display-table ?x) [?y]))
        (dolist (char '(#xA0 #x202F))
          (should (equal (h-glyph buffer-display-table char) h-nobreak-glyph)))
        (should (eq (glyph-char (display-table-slot buffer-display-table 'wrap))
                    ?⸗))
        (should (eq (glyph-char (display-table-slot buffer-display-table
                                                    'truncation))
                    ?…))))
    (should (equal (aref shared ?x) [?y]))
    (dolist (char '(#xA0 #x202F))
      (should-not (aref shared char)))
    (should-not (display-table-slot shared 'wrap))
    (should-not (display-table-slot shared 'truncation))))

(defconst h-scan-functions
  '(re-search-forward re-search-backward search-forward search-backward
    looking-at looking-at-p looking-back skip-chars-forward skip-chars-backward))

(defvar h-scans nil)

(defun h-note-scan (&rest arguments)
  (push (car arguments) h-scans))

(defun h-scans-over-a-run (length)
  "Return what the scan looks for, in order, on a line with long runs of blanks.
The line has LENGTH no-break spaces and LENGTH spaces, then text, then LENGTH
more spaces, which are the trailing whitespace."
  (h-with-whitespace-buffer
    (text-mode)
    (insert "x" (make-string length #xA0) (make-string length ?\s) "y"
            (make-string length ?\s) "\n")
    (whitespace-mode 1)
    (setq h-scans nil)
    (dolist (function h-scan-functions)
      (advice-add function :before #'h-note-scan))
    (unwind-protect
        (shields/fontify-trailing-spaces (point-min) (point-max))
      (dolist (function h-scan-functions)
        (advice-remove function #'h-note-scan)))
    (should (equal (h-marker-starts)
                   (number-sequence (+ 3 (* 2 length)) (+ 2 (* 3 length)))))
    (reverse h-scans)))

;; A regexp that repeats the blank class retries from each blank in a run and
;; backtracks over the rest of it, so its time grows with the square of the run.
;; Any other number or kind of search would also depend on the run's length.
(ert-deftest h-whitespace-scan-is-linear-in-the-length-of-a-run ()
  (let ((short (h-scans-over-a-run 10)))
    (should short)
    (dolist (pattern short)
      (should-not (string-match-p "[*+]" pattern)))
    (should (equal short (h-scans-over-a-run 20000)))))

(defun h-text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defmacro h-with-cleanup-buffer (mode enabled tabs &rest body)
  "Run BODY in a buffer in MODE, with `whitespace-mode' on if ENABLED.
TABS is the value of `indent-tabs-mode'."
  (declare (indent 3))
  `(h-with-whitespace-buffer
     (rename-buffer "h-cleanup" t)
     (funcall ,mode)
     (setq-local indent-tabs-mode ,tabs)
     (whitespace-mode (if ,enabled 1 -1))
     ,@body))

(defconst h-cleanup-text
  (concat "trail   \ntabs\t\t\nnbsp\u00A0\u00A0\nmix \t \u00A0\nnarrow\u202F\n"
          "sbt  \tx\nsat\t        y\nlast  "))

;; whitespace.el's `whitespace-trailing-regexp' has no U+202F.
(defconst h-cleaned-text
  (let ((head "trail\ntabs\nnbsp\nmix\nnarrow\u202F\n"))
    `((t . ,(concat head "sbt\tx\nsat\t\ty\nlast\n"))
      (nil . ,(concat head "sbt     x\nsat             y\nlast\n")))))

(ert-deftest h-whitespace-cleanup-trims-trailing-blanks-and-fixes-tab-spacing ()
  (should-not (default-value 'whitespace-action))
  (let ((default (default-value 'whitespace-style)))
    (dolist (mode '(text-mode makefile-mode))
      (dolist (tabs '(t nil))
        (dolist (enabled '(t nil))
          (h-with-cleanup-buffer mode enabled tabs
            (insert h-cleanup-text)
            (set-buffer-modified-p nil)
            (let ((style whitespace-style))
              (whitespace-cleanup)
              (should (equal (h-text) (alist-get tabs h-cleaned-text)))
              (should (buffer-modified-p))
              (should (eq whitespace-style style))
              (should (eq (local-variable-p 'whitespace-style)
                          (eq mode 'makefile-mode)))
              (should-not (memq 'trailing whitespace-style))
              (should (eq (default-value 'whitespace-style) default))
              (should-not (default-value 'whitespace-active-style)))))))))

(ert-deftest h-whitespace-cleanup-region-works-within-its-region ()
  (dolist (mode '(text-mode makefile-mode))
    (dolist (enabled '(t nil))
      (dolist (interactively '(nil t))
        (h-with-cleanup-buffer mode enabled t
          (insert "a \t\nb   \nc  \tx\nd\t        y\ne  \n")
          (let ((start (progn (goto-char (point-min)) (forward-line 1) (point)))
                (end (progn (forward-line 2) (line-end-position))))
            (if interactively
                (progn
                  (set-mark start)
                  (goto-char end)
                  (call-interactively #'whitespace-cleanup-region))
              (whitespace-cleanup-region start end))
            (should (equal (h-text) "a \t\nb\nc\tx\nd\t\ty\ne  \n"))))))))

(ert-deftest h-whitespace-cleanup-stays-a-command ()
  (should (equal (interactive-form 'whitespace-cleanup) '(interactive "@")))
  (should (equal (interactive-form 'whitespace-cleanup-region)
                 '(interactive "@r")))
  (should (equal (interactive-form 'whitespace-report)
                 '(interactive (list current-prefix-arg))))
  (should (equal (interactive-form 'whitespace-report-region)
                 '(interactive "r")))
  (h-with-cleanup-buffer 'text-mode t t
    (insert "a \t\nb  \n")
    (call-interactively #'whitespace-cleanup)
    (should (equal (h-text) "a\nb\n"))))

(ert-deftest h-whitespace-report-counts-trailing-blanks ()
  (dolist (mode '(text-mode makefile-mode))
    (dolist (enabled '(t nil))
      (h-with-cleanup-buffer mode enabled t
        (dolist (case '(("a \n" t) ("a\t\n" t) ("a\u00A0\n" t) ("a  \t\n" t)
                        ("a\n" nil) ("a b\n" nil)))
          (erase-buffer)
          (insert (car case))
          (should (eq (and (whitespace-report nil 'never) t) (cadr case)))
          (should (eq (and (whitespace-report-region
                            (point-min) (point-max) nil 'never)
                           t)
                      (cadr case))))
        (erase-buffer)
        (insert "a \nb\n")
        (should (whitespace-report-region 1 3 nil 'never))
        (should-not (whitespace-report-region 4 6 nil 'never))
        (should-not (memq 'trailing whitespace-style))
        (should-not (memq 'trailing (default-value 'whitespace-style)))))))

(ert-deftest h-whitespace-report-counts-trailing-blanks-where-the-mode-never-ran ()
  (h-with-whitespace-buffer
    (text-mode)
    (should-not whitespace-mode)
    (should-not whitespace-active-style)
    (insert "a \n")
    (should (whitespace-report nil 'never))
    (erase-buffer)
    (insert "a\n")
    (should-not (whitespace-report nil 'never))))

(ert-deftest h-whitespace-report-lists-trailing-blanks ()
  (h-with-cleanup-buffer 'text-mode t t
    (insert "a \n")
    (unwind-protect
        (progn
          (should (whitespace-report))
          (with-current-buffer whitespace-report-buffer-name
            (should (string-match-p "^ trailing +\\[X\\] +\\[X\\] "
                                    (buffer-string)))))
      (when (get-buffer whitespace-report-buffer-name)
        (kill-buffer whitespace-report-buffer-name)))))

(defun h-project-results (cases)
  "Visit a file for each of CASES and return what cleanup and report do to it.
Each of CASES is a list of a `whitespace-style' that a project sets, where it
sets it (`dir' for a .dir-locals.el, `file' for the file's first line), the
file's name and its text.  Each result is the text after cleanup and whether the
report found a problem."
  (let* ((noninteractive nil)
         (enable-local-variables :all)
         (dir (make-temp-file "h-whitespace" t))
         (index 0))
    (unwind-protect
        (mapcar
         (lambda (case)
           (pcase-let* ((`(,style ,where ,name ,text) case)
                        (subdir (expand-file-name (format "%d" (cl-incf index))
                                                  dir))
                        (file (expand-file-name name subdir)))
             (make-directory subdir)
             (when (eq where 'dir)
               (with-temp-file (expand-file-name ".dir-locals.el" subdir)
                 (insert (format "((nil . ((whitespace-style . %S))))" style))))
             (with-temp-file file
               (when (eq where 'file)
                 (insert (format "-*- whitespace-style: %S -*-\n" style)))
               (insert text))
             (with-current-buffer (find-file-noselect file)
               (unwind-protect
                   (progn
                     (setq-local indent-tabs-mode t)
                     (should whitespace-mode)
                     (should (equal whitespace-style style))
                     (should (eq whitespace-active-style whitespace-style))
                     (let ((report (and (whitespace-report nil 'never) t)))
                       (whitespace-cleanup)
                       (goto-char (point-min))
                       (when (eq where 'file)
                         (forward-line 1))
                       (list (buffer-substring-no-properties (point)
                                                             (point-max))
                             report)))
                 (set-buffer-modified-p nil)
                 (kill-buffer)))))
         cases)
      (delete-directory dir t))))

(ert-deftest h-whitespace-a-project-style-is-used-as-given ()
  (let ((text "x  \ty \n"))
    (should (equal
             (h-project-results
              (cl-loop for style in '(nil (face space-before-tab) (face trailing)
                                      (face))
                       nconc (cl-loop for where in '(dir file)
                                      nconc (mapcar (lambda (name)
                                                      (list style where name text))
                                                    '("a.txt" "Makefile")))))
             (cl-loop for result in `((,text nil) ("x\ty \n" t) ("x  \ty\n" t)
                                      (,text nil))
                      nconc (list result result result result))))
    (should (equal (h-project-results
                    '(((face space-before-tab) dir "a.txt" "a \n")))
                   '(("a \n" nil))))))

(defvar h-seen nil)

(ert-deftest h-whitespace-a-failure-in-a-command-leaves-the-style-alone ()
  (let ((default (default-value 'whitespace-style))
        (other (generate-new-buffer "h-other")))
    (unwind-protect
        (dolist (mode '(text-mode makefile-mode))
          (dolist (enabled '(t nil))
            (dolist (case '((whitespace-cleanup whitespace-replace-action)
                            (whitespace-cleanup-region whitespace-replace-action)
                            (whitespace-report whitespace-kill-buffer)
                            (whitespace-report-region whitespace-kill-buffer)))
              (dolist (failure '(error quit))
                (h-with-cleanup-buffer mode enabled t
                  (insert "a \n")
                  (let ((style whitespace-style)
                        (active whitespace-active-style)
                        (local (local-variable-p 'whitespace-style))
                        (active-local (local-variable-p 'whitespace-active-style))
                        (arguments (and (string-suffix-p
                                         "-region" (symbol-name (car case)))
                                        (list (point-min) (point-max)))))
                    (setq h-seen nil)
                    (cl-letf (((symbol-function (cadr case))
                               (lambda (&rest _)
                                 (push (list whitespace-style
                                             whitespace-active-style
                                             (default-value 'whitespace-style)
                                             (buffer-local-value
                                              'whitespace-style other))
                                       h-seen)
                                 (if (eq failure 'quit)
                                     (signal 'quit nil)
                                   (error "Injected")))))
                      (should (eq (condition-case nil
                                      (apply (car case) arguments)
                                    (error 'error)
                                    (quit 'quit))
                                  failure)))
                    (should (= (length h-seen) 1))
                    (pcase-let ((`(,inside ,inside-active ,inside-default
                                           ,inside-other)
                                 (car h-seen)))
                      (should (equal inside (cons 'trailing style)))
                      (should (equal inside-active
                                     (and active (cons 'trailing style))))
                      (should (eq inside-default default))
                      (should (eq inside-other default)))
                    (should (eq whitespace-style style))
                    (should (eq whitespace-active-style active))
                    (should (eq (local-variable-p 'whitespace-style) local))
                    (should (eq (local-variable-p 'whitespace-active-style)
                                active-local))
                    (should (shields/own-whitespace-style-p))
                    (should (eq (default-value 'whitespace-style) default))
                    (should (eq (buffer-local-value 'whitespace-style other)
                                default))
                    (should-not (default-value 'whitespace-active-style))))))))
      (kill-buffer other))))

(ert-deftest h-whitespace-nested-commands-add-trailing-once ()
  (h-with-cleanup-buffer 'text-mode t t
    (insert "a \n")
    (let ((style whitespace-style) seen)
      (cl-letf (((symbol-function 'whitespace-replace-action)
                 (lambda (&rest _) (push whitespace-style seen))))
        (whitespace-cleanup))
      (should seen)
      (dolist (inside seen)
        (should (equal inside (cons 'trailing style)))))))

(defun h-file-text (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest h-whitespace-auto-cleanup-trims-on-save ()
  (h-with-visited-file "a.txt" "x\n"
    (setq-local whitespace-action '(auto-cleanup)
                indent-tabs-mode t)
    (should (memq #'whitespace-write-file-hook write-file-functions))
    (erase-buffer)
    (insert "a \t\nb  \tx\nc \n")
    (save-buffer)
    (should (equal (h-text) "a\nb\tx\nc\n"))
    (should (equal (h-file-text buffer-file-name) "a\nb\tx\nc\n"))
    (should-not (buffer-modified-p))))

(ert-deftest h-whitespace-cleanup-action-trims-when-the-mode-starts ()
  (h-with-cleanup-buffer 'text-mode nil t
    (insert "a \t\nb  \tx\n")
    (setq-local whitespace-action '(cleanup))
    (whitespace-mode 1)
    (should (equal (h-text) "a\nb\tx\n"))))

(ert-deftest h-whitespace-report-on-bogus-reports-trailing-blanks ()
  (dolist (case '(("a \n" t) ("a\n" nil)))
    (h-with-cleanup-buffer 'text-mode nil t
      (insert (car case))
      (setq-local whitespace-action '(report-on-bogus))
      (when (get-buffer whitespace-report-buffer-name)
        (kill-buffer whitespace-report-buffer-name))
      (unwind-protect
          (progn
            (whitespace-mode 1)
            (should (eq (and (get-buffer whitespace-report-buffer-name) t)
                        (cadr case))))
        (when (get-buffer whitespace-report-buffer-name)
          (kill-buffer whitespace-report-buffer-name))))))

(ert-deftest h-whitespace-abort-on-bogus-refuses-to-save-trailing-blanks ()
  (h-with-visited-file "a.txt" "x\n"
    (setq-local whitespace-action '(abort-on-bogus))
    (erase-buffer)
    (insert "a \n")
    (let ((error (should-error (save-buffer))))
      (should (string-match-p "Abort write due to whitespace problems"
                              (error-message-string error))))
    (should (equal (h-file-text buffer-file-name) "x\n"))
    (erase-buffer)
    (insert "a\n")
    (should-not (whitespace-write-file-hook))
    (save-buffer)
    (should-not (buffer-modified-p))))

;; A batch frame displays no color, so no `supports' condition holds in it and
;; only the `min-colors' ones decide.  test_emacs_render.py runs terminals that
;; do display the colors the `supports' conditions name.
(ert-deftest h-whitespace-faces-fit-the-terminal-palette ()
  (should-not (display-supports-face-attributes-p '(:foreground "#949494")))
  (dolist (case '((16777216 ("#365773" "#e8f4ff") "#949494")
                  (256 ("#365773" "#e8f4ff") "cyan")
                  (16 ("#365773" "#e8f4ff") "cyan")
                  (8 ("blue" "white") "cyan")))
    (pcase-let ((`(,cells ,colors ,gray) case))
      (should (equal (h-theme-colors 'shields/whitespace cells
                                     :foreground :background)
                     colors))
      (should (equal (h-theme-colors 'shields/nobreak-marker cells
                                     :foreground :inherit)
                     (list gray 'fixed-pitch)))
      (dolist (face '(shields/line-marker whitespace-tab))
        (should (equal (h-theme-colors face cells :inherit)
                       '(shields/whitespace)))))))

(ert-deftest h-whitespace-face-and-wrap-defaults ()
  (should (eq (glyph-char (display-table-slot standard-display-table 'wrap)) ?⸗))
  (should (eq (glyph-char (display-table-slot standard-display-table 'truncation)) ?…))
  (should-not (default-value 'truncate-lines))
  (should-not (default-value 'truncate-partial-width-windows))
  (dolist (face '(shields/whitespace shields/line-marker whitespace-tab))
    (should (equal (face-attribute face :family nil t) "CommitMonoShields Nerd Font"))
    (dolist (attribute '(:foreground :background))
      (should (equal (face-attribute face attribute nil t)
                     (face-attribute 'shields/whitespace attribute nil t))))))

(ert-run-tests-batch-and-exit "^h-whitespace-")
"""


@pytest.mark.parametrize("scenario", ["linux", "macos"])
def test_whitespace_display(scenario: str, tmp_path: Path) -> None:
    _ = run_session(scenario, tmp_path, WHITESPACE_TESTS)
