<!--
Copyright © 2025-2026 Michael Shields

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# AGENTS.md

## Code Style

- Lexical binding, use-package based, use keymap-set
- Set faces in `shields-theme.el` or a package's `:custom-face`, never with
  `custom-set-faces`: Custom copies faces set that way into the untracked
  `~/.emacs.d/custom.el`, which loads before the init files and is never
  refreshed.

## Conventions

- Do not optimize Emacs startup time - use emacsclient
- Prefer tree-sitter modes when available

## Baseline and terminal-only builds

The baseline is Emacs 31.1: `emacs-plus@31` on macOS, and Homebrew's core
`emacs` on Linux. Older Emacs, such as Debian's 30.1, is not supported. The
Linux build has no window system, no SQLite and no native compilation, so init
must finish there.

- Functions that exist only with a window system are void in the Linux build
  (`tool-bar-mode`, `set-fringe-mode`, `set-fontset-font`, ...): guard each
  call with `fboundp` or `featurep`.
- Gate macOS-only code on `(eq system-type 'darwin)`, or on `(featurep 'ns)` for
  functions that only an ns build has. Never gate on `display-graphic-p` or
  `window-system`: the macOS daemon is headless when init runs.
- Gate a package that is unavailable or useless on Linux by wrapping its
  `use-package` form in `when`. `:if`, `:when` and `:unless` do not stop straight
  from installing it, because its `:straight` handler runs before them.
- Gate anything that needs SQLite on `(sqlite-available-p)`. Today that is only
  forge and its dependencies, emacsql and closql.
- A face spec that names an NSColor (`selectedTextBackgroundColor`) goes under
  `((type ns))`, with a `t` entry that gives a color a terminal can show.
- A terminal cannot send `s-` keys, F19 or `C-<backspace>`, and sends `C-=` and
  `C--` only with modifyOtherKeys, so `init-keybindings.el` binds a global `C-c`
  twin to each command they run: `i`, `SPC`, `o`, `.`, `,`, `j`, `=`, `-` and
  `J`. Give a new binding of that kind a twin too. A major mode can shadow a
  twin: Markdown has `C-c -` and C has `C-c .`.
- `tests/test_emacs_tty.py` loads every file in `lisp/` without the packages
  they configure, once as the Linux build and once as macOS, and checks the
  guards and keys above. A top-level call to a function from a package that the test does not
  install needs a stub in its harness, and a `:config` body runs only for a
  package whose feature the harness provides.
