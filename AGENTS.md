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
  added to the script, not a skip. Node and zsh tests have no skip mechanism;
  keep it that way. The script pins Lima with a checksum; bump it by hand.
  `zizmor.yml` audits the workflows.
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
starting with `.` (e.g. `.claude/`, `.zshrc`), except `.github/`, which holds
GitHub-specific repository configuration. Untracked files are not copied. Files
are copied, not symlinked, so this repo is the source of truth; edits take
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
  environment or a message. `limavm new` asks the Mac's `vm-access` gateway to
  authorize the selected repository, registers an immutable VM ID and reverse
  Unix socket, installs the gateway CA and local relay in the guest, and grants
  the repository. `--no-repo` skips the grant. The Mac keeps GitHub App tokens
  and refreshes them on demand; the guest gets only a dummy credential helper,
  a dummy `GH_TOKEN`, the CA and its ID. The gateway inspects Git push commands,
  allows only `task/VM-ID/*`, and applies live per-VM/repository grants to REST
  and GraphQL reads. `setup-github-gateway` puts `task/VM-ID/*` in the agents'
  managed instructions. `limavm github` adds a grant and `limavm rm` disables
  the VM before deletion. `gateway/` contains the shared policy, service, state,
  authorization and CLI. `.config/vm-access/profiles.yaml` defines templates;
  edits affect future grants only. On the Mac, `provision.sh dev` bundles the
  service and loads its launchd agent. The agent's `main.mjs` is built from this
  checkout, not copied by `provision.sh`.
  `bin/github_app_token.py` remains for non-gateway throwaway machines; Lima
  bases run `provision/throwaway.sh --no-github-timer`. `limavm` never calls
  `security`.
  `limavm claude-token` keeps the Claude token in
  `~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` on the Mac (the path the guest
  uses; `.zshrc` exports it only on Linux, and the Mac sandbox denies reads
  there), and `limavm new` installs it from that file when it exists, and
  otherwise asks and points to `claude-token`. It also
  sends the Mac's `~/.codex/auth.json` (which exists only with
  `cli_auth_credentials_store = "file"`; the Keychain holds the login otherwise)
  to `setup-secrets CODEX_AUTH`, which requires a JSON object and writes the
  same path in the guest; `--no-codex-auth` leaves Codex to
  `codex login --device-auth` in the VM.
- `cloudflare/wrangler.jsonc` deploys `cloudflare/worker.ts` and the agent
  image through `cloudflare/deploy.sh`, which stages and scans the tree, adds
  this Mac's SSH public key, and installs an admin secret through Wrangler.
  `cloudflare/authority.ts` holds GitHub App authorizations, live grants and
  audit records in one SQLite Durable Object. `cloudflare/outbound.ts` applies
  `gateway/policy.ts` to intercepted container traffic. The container receives
  no GitHub token; `provision/cloudflare-agent.sh` installs its dummy credential
  helper and `cloudflare/entrypoint.sh` installs Cloudflare's runtime CA.
  `bun run cloudflare/admin.ts` manages authorization, VMs, grants and logs.
  Its device flow issues a separate authorization from Lima's Mac gateway.
- `provision/throwaway.sh` (run as root, also by the Cloudflare image) makes a
  machine a throwaway environment: `/etc/dotfiles-throwaway`, Claude Code's
  managed settings (generated with jq from the deny rules, the `git_guard.py`
  hook and the status line in `.claude/settings.json`, so add to that file, not
  the output), and Codex's system config and rules. `.zshrc`'s `c` tests the
  marker, which `DOTFILES_THROWAWAY_MARKER` can point elsewhere in a test, and
  must stay a builtin-only test. `provision/reset-identity.sh` removes the
  agents' installation identifiers before a base is cloned; add a key there when
  an agent stores another.
- Per-OS copy excludes: macOS leaves out `bin/setup-secrets`,
  `bin/github_app_token.py`, `bin/setup-github-gateway` and
  `bin/github-gateway-credential`; Linux leaves out `bin/limavm` and
  `bin/vm-access`.
- `tests/test_limavm.zsh` and `tests/test_setup_secrets.zsh` use stubs and run
  anywhere. `gateway/*.test.ts` cover the policy, token refresh, live grants,
  audit and HTTPS Unix socket. `tests/fake_github.py` and
  `tests/test_github_app_token.py` still cover the non-gateway token manager.
  `tests/test_lima_dev_yaml.py` needs `limactl`, so it skips in the Linux image
  (CI installs it).
  No automated test covers what only a running VM shows: the network
  isolation, relay and agent behavior under the installed managed settings.

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
