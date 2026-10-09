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
  `MODULES` selects Brewfile modules (`dev` by default, and `make test lint`
  needs its tools). The first run takes tens of minutes.
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

- macOS also copies `Library/`, and leaves out `bin/setup-secrets` and
  `bin/github_app_token.py`.
- Linux leaves out the files that only a Mac can use. The list is `copy_paths`
  in `provision.sh`, and `tests/test_provision.py` checks it, so a new
  macOS-only file needs an entry in both.
- The macOS-only steps are functions in `provision/macos.sh`, which
  `provision.sh` calls in the order a macOS run needs them. Call them as plain
  statements: inside `fn &&`, `fn ||` or `if fn`, errexit is off. The apt
  packages, locale and login shell on Linux are `provision/linux-system.sh`, run
  with sudo.
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

Throwaway Lima VMs (the README describes them for users):

- `lima/dev.yaml` is the VM definition, and `bin/limavm` (Mac only, bash 3.2)
  builds `dotfiles-base` from it and clones it. `bin/setup-secrets` runs in the
  guest and reads a secret from stdin; no secret may reach an argument list, an
  environment or a message. `limavm new --repo` runs the device flow of a GitHub
  App (no private key) on the Mac and sends the record to
  `setup-secrets GITHUB_APP_AUTH`. `bin/github_app_token.py` (Linux only; on
  Python 3.13, which Debian's `python3` is) keeps the record fresh from a
  systemd timer that `provision/throwaway.sh` installs and from git's credential
  helper. The Mac keeps no GitHub secret, and `limavm` never calls `security`.
  `limavm claude-token` keeps the Claude token in
  `~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` on the Mac (the path the guest
  uses; `.zshrc` exports it only on Linux, and the Mac sandbox denies reads
  there), and `limavm new` installs it from that file instead of asking. It also
  sends the Mac's `~/.codex/auth.json` to `setup-secrets CODEX_AUTH`, which
  requires a JSON object and writes the same path in the guest; `--no-codex-auth`
  leaves Codex to `codex login --device-auth` in the VM.
- `provision/throwaway.sh` (run as root, also by the Cloudflare image) makes a
  machine a throwaway environment: `/etc/dotfiles-throwaway`, Claude Code's
  managed settings (generated with jq from the deny rules, the `git_guard.py`
  hook and the status line in `.claude/settings.json`, so add to that file, not
  the output), and Codex's system config and rules. `.zshrc`'s `c` tests the
  marker, which `DOTFILES_THROWAWAY_MARKER` can point elsewhere in a test, and
  must stay a builtin-only test. `provision/reset-identity.sh` removes the
  agents' installation identifiers before a base is cloned; add a key there when
  an agent stores another.
- Per-OS copy excludes: macOS leaves out `bin/setup-secrets` and
  `bin/github_app_token.py`, and Linux leaves out `bin/limavm`.
- `tests/test_limavm.zsh` and `tests/test_setup_secrets.zsh` use stubs and run
  anywhere, with `tests/fake_github.py` as the GitHub of the device flow.
  `tests/test_github_app_token.py` runs the token manager and the real `gh` and
  `git` against that fake, also in the Linux image. `tests/test_lima_dev_yaml.py`
  needs `limactl`, so it skips in the Linux image. No test covers what only a
  running VM shows: the network isolation, what the agents may do under the
  managed settings, and the systemd timer.

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
