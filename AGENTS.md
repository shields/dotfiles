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
- **CI**: `.github/workflows/ci.yml` runs `make lint build test` on
  ubuntu-26.04 as the "Lint and test" check, after `make ci-tools`
  (`tools/install_ci_tools.sh`) installs the tools the tests need and
  `make setup` installs the locked dependencies. `DOTFILES_FAIL_ON_SKIP=1` makes
  any skipped pytest test fail the run, so a test that needs a new tool needs it
  added to the script, not a skip. Zsh tests have no skip mechanism; keep it
  that way. `zizmor.yml` audits the workflows.
- **Formatting**: `make lint` runs `prettier --check .` and `make fmt` runs
  `prettier --write .`, so both cover every file type Prettier supports natively
  (not TOML, which needs a plugin) except what `.gitignore` and
  `.prettierignore` list. A Prettier bump that changes the output fails CI until
  `make fmt` is run.
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
`uname -s`; any other OS is an error. After Homebrew installs its base module,
chezmoi installs the files in `home/`, selected by `.chezmoiroot`. The README
lists their source and destination paths.
`make diff` previews these files and `make apply` installs them independently
of provisioning. Both use this checkout explicitly. Existing conflicts fail,
including on first adoption; the README describes interactive resolution.
Chezmoi reads uncommitted and untracked files in `home/`, so keep only deployable
configuration there. Test rendered output in temporary homes with chezmoi's
config, cache and persistent state isolated too (`tests/test_provision.py`).

It installs the remaining dotfiles by piping
`git ls-files` through `tar` into `$HOME`: `bin/` and every git-tracked path
starting with `.` (e.g. `.claude/`, `.zshrc`), except `.github/`, which holds
GitHub-specific repository configuration. The copy excludes chezmoi metadata
and its managed paths, so each path has one installer. Untracked files
are not copied by tar. Files are copied, not symlinked, so this repo is the
source of truth; edits take effect after `make apply` for the chezmoi files or
`./provision.sh` for the rest.

- macOS also copies `Library/`.
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
- Check whether a change is needed before invoking `sudo`. Provisioning steps
  that are already satisfied must not invoke `sudo`, including for temporary
  files, cleanup, or permissions that are already correct.

Most machines keep everything on one volume, but one Mac's `~/src` is a symlink
to `/Volumes/Data/src` on an external drive. Configuration must work in both
layouts; `.codex/README.md` describes how the uv cache and Codex worktrees
follow the checkouts there.

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
gitleaks finds a secret there. `make test-linux` builds `tools/linux-test.Dockerfile`
from such a copy, which runs `./provision.sh` in a Debian 13 image.

Throwaway VMs belong to `cog`, in shields/cog; the README states the contract.

- `provision/claude-model-pricing.json` holds the `modelPricing` rates for the
  models `cg` uses, which Claude Code reads only from managed settings.
  `provision/linux-system.sh` installs it in
  `/etc/claude-code/managed-settings.d/` beside the `managed-settings.json` that
  `cog` writes, and `macos.sh`'s `macos_claude_model_pricing` in
  `/Library/Application Support/ClaudeCode/`. A model ID change in `cg` needs
  the same change there, without the `[1m]` suffix that Claude Code ignores when
  it matches prices; `tests/test_zsh_startup.py` checks that they agree.
- `cog` reads `./provision.sh`, `.claude/settings.json` (the `git push` deny
  rules, the `git_guard.py` hook and the status line) and
  `tools/configure_codex.py` from a checkout, and this repository reads the
  markers `/etc/cog-throwaway` and `/etc/cog-github-gateway`. Changing any of
  them changes the contract in the README, and `cog` needs the same change.
  `.zshrc`'s `_claude_start`, which `c` and `cg` share, tests the throwaway
  marker, which `DOTFILES_THROWAWAY_MARKER` can point elsewhere in a test, and
  must stay a builtin-only test.

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
