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
- **Linux tests**: `make test-linux` provisions a Debian 13 image in Docker and
  runs `make test lint` inside it, so it also covers uncommitted changes.
  `MODULES` selects Brewfile modules (`dev` by default). The first run takes
  tens of minutes.
- **Fonts**: `tools/create_nerd_commit_mono.sh` rebuilds the Nerd Font in
  `Library/Fonts/` from `commit-mono/` (checked by `make test`)
- **Benchmark**: `make bench` times `wt` with hyperfine in a throwaway repo;
  `zsh tools/bench_wt.zsh --rc` adds the interactive shell's chpwd hooks, and
  repeated `-s` compares implementations
- **Shell startup**: `make bench-startup` times a login shell to its first
  prompt and profiles where the time goes; `zsh tools/bench_startup.zsh`'s
  repeated `-s` compares `.zshrc` candidates before provisioning them, and
  `--ready` includes all deferred initialization

## Deployment

`provision.sh` runs on macOS and on Debian Linux, which it tells apart with
`uname -s`; any other OS is an error. It installs the dotfiles by piping
`git ls-files` through `tar` into `$HOME`: `bin/` and every git-tracked path
starting with `.` (e.g. `.claude/`, `.zshrc`). Untracked files are not copied.
Files are copied, not symlinked, so this repo is the source of truth; edits take
effect only after running `./provision.sh`.

- macOS also copies `Library/`, and leaves out `bin/setup-secrets`.
- Linux leaves out the files that only a Mac can use. The list is `copy_paths`
  in `provision.sh`, and `tests/test_provision.py` checks it, so a new
  macOS-only file needs an entry in both.
- The macOS-only steps are functions in `provision/macos.sh`, which
  `provision.sh` calls in their original order. Call them as plain statements:
  inside `fn &&`, `fn ||` or `if fn`, errexit is off. The apt packages, locale
  and login shell on Linux are `provision/linux-system.sh`, run with sudo.
- `provision.sh` and everything else that runs on macOS must work in bash 3.2:
  no `mapfile`, associative arrays or `${var,,}`, and no expansion of an empty
  array under `set -u`.

Homebrew packages are modules, `brew/*.Brewfile`. The `Brewfile` in the root is
a loader for `base`, `macos` or `linux`, and the selected optional modules.
`./provision.sh [--remove-modules] [MODULE... | none]` saves the selection in
`~/.config/dotfiles/brew-modules` (the logic is in `provision/modules.sh`) and
refuses to drop an installed module without `--remove-modules`. A new module is
installed only where it has been named once. Add packages with
`brew bundle add --file=brew/<module>.Brewfile`. Never run `brew bundle dump`:
it would overwrite the loader. The README describes the modules.

`tools/stage_tree.sh DEST` copies the working tree (tracked files and untracked
files that are not ignored) into a new git repository at `DEST` and fails if
gitleaks finds a secret there. `make test-linux` builds `cloudflare/Dockerfile`
from such a copy, which runs `./provision.sh` in a Debian 13 image.

Shared agent instructions live in `.agents/AGENTS.md`. The global
`~/.claude/CLAUDE.md` imports them with `@../.agents/AGENTS.md` and adds
Claude-specific instructions. Provisioning concatenates `.agents/AGENTS.md`
and `.codex/instructions.md` into the global `~/.codex/AGENTS.md`, since Codex
does not expand `@` imports. Edit those source files to change Codex's
instructions; the installed file is generated.

## Code Style

- **Shell Scripts**: set -euo pipefail, prefer absolute paths
  - In Zsh, never use lowercase `path` as a local or general-purpose variable:
    it is a special array tied to `PATH`, so shadowing it can break command
    lookup and `chpwd` hooks. Use a descriptive name such as `worktree_path`.
