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

HARNESS = r""";;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'use-package)

(defvar h-repo (getenv "DOTFILES_EMACS_DIR"))
(defvar h-macos (equal (getenv "DOTFILES_EMACS_SCENARIO") "macos"))

(when (< emacs-major-version 31)
  (error "These files need Emacs 31, not %s" emacs-version))

(setq native-comp-jit-compilation nil
      native-comp-enable-subr-trampolines nil
      user-emacs-directory (file-name-as-directory (getenv "HOME"))
      custom-theme-directory h-repo)
(add-to-list 'load-path (expand-file-name "lisp" h-repo))
(add-to-list 'custom-theme-load-path h-repo)

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
    (region_background . ,(face-attribute 'region :background nil)))))
"""

type Report = dict[str, object]


def run_session(scenario: str, home: Path) -> Report:
    emacs = shutil.which("emacs")
    assert emacs, "emacs is required"
    harness = home / "harness.el"
    _ = harness.write_text(HARNESS, encoding="utf-8")
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
