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

## Commands

- **Emacs Setup**: `emacs --batch --script .emacs.d/provision.el`
- **Fonts**: `tools/create_nerd_commit_mono.sh` rebuilds the Nerd Font in
  `Library/Fonts/` from `commit-mono/` (checked by `make test`)

## Deployment

`provision.sh` installs the dotfiles by piping them through `tar` into `$HOME`:
`bin/`, `Library/`, and every git-tracked path starting with `.` (e.g.
`.claude/`, `.zshrc`). Files are copied, not symlinked, so this repo is the
source of truth; edits take effect only after running `./provision.sh`. Because
`.claude/` is copied, `.claude/CLAUDE.md` here is installed as the global
`~/.claude/CLAUDE.md`.

## Code Style

- **Emacs Lisp**: Lexical binding, use-package based, use keymap-set
- **Shell Scripts**: set -euo pipefail, prefer absolute paths
  - In Zsh, never use lowercase `path` as a local or general-purpose variable:
    it is a special array tied to `PATH`, so shadowing it can break command
    lookup and `chpwd` hooks. Use a descriptive name such as `worktree_path`.

## Project Conventions

- Do not optimize Emacs startup time - use emacsclient
- Prefer tree-sitter modes when available
