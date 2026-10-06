<!--
Copyright © 2020, 2022, 2026 Michael Shields

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

# macOS provisioning

1. `git clone https://github.com/shields/dotfiles.git ~/src/github.com/shields/dotfiles`
   (macOS offers to install git with the command line tools the first time you
   run it)
1. `cd ~/src/github.com/shields/dotfiles`
1. `./provision.sh`
1. Reboot

Additional steps not yet automated:

- Open Karabiner-Elements and grant permissions
- Open Chrome and sign in
- Open System Preferences:
  - Users & Groups > Login Items: add Karabiner-Elements

Many preferences can be translated to `defaults write` settings using
`tools/diff_defaults.py`.

# Brewfile modules

What `provision.sh` installs with Homebrew is split into modules,
`brew/<module>.Brewfile`. The `Brewfile` in the root only loads `base`, the
operating system's own module (`macos` or `linux`) and the optional modules
that are selected.

| Module  | Contents                                                                                                       |
| ------- | -------------------------------------------------------------------------------------------------------------- |
| `base`  | Always installed: git, gh, shell tools, Python, Node, Go, uv, the Claude Code and Codex casks, and lgtmcp.     |
| `dev`   | Linters, formatters, language servers, compilers and other development tools, and the cargo, go and npm tools. |
| `cloud` | Cloud and Kubernetes command-line tools, including the Google Cloud CLI.                                       |
| `data`  | Databases, notebooks, mail and sync tools.                                                                     |
| `media` | Image, audio and video tools.                                                                                  |
| `macos` | Always installed on macOS: casks, Mac App Store apps, GNU tools, Emacs Plus and other macOS-only packages.     |
| `linux` | Always installed on Linux: Emacs.                                                                              |

```
./provision.sh [--remove-modules] [MODULE... | none]
```

- The selection is saved in `~/.config/dotfiles/brew-modules`, as a
  space-separated list that may be empty. Without arguments, `./provision.sh`
  uses the saved selection. Where none is saved, macOS installs every optional
  module and Linux installs `dev`.
- Module names on the command line replace the saved selection. `none` selects
  no optional modules.
- Naming fewer modules than are installed would uninstall their packages, so
  `provision.sh` stops and prints what `brew bundle cleanup` would uninstall.
  Pass `--remove-modules` to go ahead.
- A module added to `brew/` is not installed on a machine that has a saved
  selection until you name it once, for example
  `./provision.sh dev cloud newmodule`.
- To change what a module installs, edit `brew/<module>.Brewfile`, or run
  `brew bundle add --file=brew/<module>.Brewfile ...` (or `remove`).
- To check a machine against its modules, run `brew bundle check --verbose` and
  `brew bundle cleanup` (without `--force`) in an interactive shell. The cargo
  entries are found only where rustup's `bin` is on the `PATH`, which `.zshrc`
  arranges.
- Do not run `brew bundle dump`: it would replace the loader with a flat list.

# Persistent Linux box

A Linux machine that you reach over SSH gets the same shell, git, Emacs (in the
terminal) and agent setup as the Mac. It needs Debian 13 on x86_64 or arm64 and
a regular user with sudo; Homebrew refuses to run as root.

1. `sudo apt update && sudo apt install git tmux`
1. `git clone https://github.com/shields/dotfiles.git ~/src/github.com/shields/dotfiles`
1. `cd ~/src/github.com/shields/dotfiles`
1. Run `tmux`, then `./provision.sh [MODULE...]` inside it, so that a dropped
   connection does not stop a run that takes tens of minutes. The default is the
   `dev` module; see Brewfile modules above.
1. If sudo asks for a password, expect two prompts: one at the start, for the
   packages that `provision/linux-system.sh` installs from apt, and a second one
   near the end, for the libraries that Playwright's browser needs.

`provision.sh` makes Debian's `/usr/bin/zsh` the login shell, generates the
`en_US.UTF-8` locale and installs Homebrew in `/home/linuxbrew/.linuxbrew`.
Running it again is safe, and takes a few minutes.

Nothing secret is in the repository, so log in once on each machine:

- GitHub: `gh auth login`, or a fine-grained token that is limited to the
  repositories you need: `gh auth login --with-token`. git uses `gh` as its
  credential helper.
- Claude Code: `claude auth login`, or run `claude setup-token` and put the
  token in `~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` (mode 0600). Interactive
  zsh shells export it from there.
- Codex: `codex login --device-auth`

Login shells set `LANG` to `en_US.UTF-8` when the SSH client did not send one,
but a non-interactive `ssh box command` runs without a `LANG` unless the client
sends it (`SendEnv LANG`). The system `ssh_config` of Debian and of macOS's own
`/usr/bin/ssh` does, but Homebrew's `ssh`, which `provision.sh` installs and
`.zshrc` puts first in `PATH` on a Mac, does not.

# Throwaway Lima VMs

A throwaway VM is a Debian 13 machine on this Mac (Lima 2.2.1 or later, vz, arm64)
where Claude Code and Codex run with no sandbox and no permission prompts. The
restrictions that remain are the ones on state outside the VM: the deny rules,
the `git_guard.py` hook and the network isolation below. You push from inside the
VM yourself, with a token that you create for it. A VM comes from a base image
that has the same shell, git, Emacs and agent setup as the Mac, so making one
takes about a minute, and `limavm rm` discards it.

```
limavm base [MODULE...]   # once, and again to refresh the base: tens of minutes
limavm new [NAME]         # a clone of the base, with secrets, and a shell in it
limavm rm NAME
limavm list
```

`limavm base` must run in a dotfiles checkout, and builds `dotfiles-base` from
`lima/dev.yaml` and that checkout, including files that are not committed
(`tools/stage_tree.sh` copies them, and gitleaks scans the copy). It runs
`./provision.sh MODULE...` in the guest (`dev` by default), then
`provision/throwaway.sh`, forgets the identifiers that the agents stored, stops
the VM and protects it from deletion. `limavm new` clones the base (an APFS
copy-on-write clone, so it needs little disk), upgrades the Claude Code and Codex
casks before any secret is present, and then installs the secrets with
`bin/setup-secrets`, which reads each value from stdin. `ssh lima-NAME` also
works, through `~/.lima/*/ssh.config`. `LIMAVM_CPUS` and `LIMAVM_MEMORY` (in GiB)
override the size in `lima/dev.yaml` (6 CPUs and 16 GiB), and `LIMAVM_BASE`
replaces the name `dotfiles-base`.

## Secrets

Create these two Keychain items once. The command prompts for the value, so it
stays out of your shell history, and `limavm new` stops with this command if an
item is missing:

```
security add-generic-password -a "$USER" -s limavm-GH_TOKEN -w
security add-generic-password -a "$USER" -s limavm-CLAUDE_CODE_OAUTH_TOKEN -w
```

- `limavm-GH_TOKEN` holds a fine-grained personal access token. Create one for
  each class of VM, at github.com/settings/personal-access-tokens: "Only select
  repositories", with just the repositories that VMs of that class work on;
  repository permissions Contents and Pull requests set to read and write, and
  nothing else; and an expiry of days, not months. Revoke it on the same page when
  you are done with the class. `setup-secrets` logs `gh` in with it (the git
  credential helper from `provision.sh` then works), and exports nothing.
- `limavm-CLAUDE_CODE_OAUTH_TOKEN` holds the output of `claude setup-token`. It
  goes to `~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` (mode 0600), which `.zshrc`
  exports in interactive shells.
- `~/.config/lgtmcp/config.yaml` on the Mac is copied to the same place in the VM.
- Codex has no secret to copy: run `codex login --device-auth` in each VM, after
  you turn on device code login in ChatGPT's security settings.

## What is isolated

Isolated, with `lima/dev.yaml` as the source:

- No host directory is mounted. `lima/dev.yaml` sets `mounts: []`, and
  `limavm base` fails if `~/.lima/_config` has added any.
- No guest port is forwarded to the Mac. Lima still opens `127.0.0.1` ports on the
  Mac for SSH and for its DNS resolver, which other programs on the Mac can
  reach; the VM can reach only the resolver, through its DNS server.
- The SSH agent is not forwarded, and the proxy environment is not propagated.
- An nftables filter in the guest rejects connections to the Mac
  (`192.168.5.2`, also `host.lima.internal`, whose other ports reach the Mac's
  loopback), to the private ranges `10.0.0.0/8`, `172.16.0.0/12` and
  `192.168.0.0/16`, to `100.64.0.0/10`, to `169.254.0.0/16` and to `fc00::/7`.
  The VM may use only DNS and DHCP at `192.168.5.2`. The rules load before the
  network comes up on every boot, and Lima rewrites them on every boot.

Not isolated:

- The internet. The agents need it, so HTTPS and every other public address work,
  and so does DNS, through Lima's resolver on the Mac.
- Anything with root in the VM. `sudo` needs no password, so a process there can
  delete the filter. It stops accidents and code that is not root, not a
  determined attacker.
- The secrets in the VM. The agents there can read the token files, `gh`'s
  credentials and the LGTMCP key, and send them to any public address. The
  token's repository scope and expiry are what limit that.
- The Mac's own public address, if it has one. Only the private ranges are
  rejected.

## What the agents may do

`provision/throwaway.sh` makes a machine a throwaway environment. It creates
`/etc/dotfiles-throwaway`, which `.zshrc` tests (without a fork) and only on
Linux; the Mac and a persistent Linux box have no such file and keep their
sandboxed, prompting behavior.

- Claude Code gets `/etc/claude-code/managed-settings.json`, which no other
  settings file overrides. It starts in `bypassPermissions` mode and disables the
  sandbox. In that mode ask rules and a hook's ask still prompt, so managed
  settings are the only source of permission rules and hooks, and carry the
  `permissions.deny` rules, the `git_guard.py` hook and the status line from
  `.claude/settings.json`. The ask rules (package installs, `git commit`) and
  `dependency_guard.py` therefore do not apply, and neither do hooks from a
  project's own `.claude/settings.json`. In `c`, the usual `--permission-mode=auto`
  and sandbox settings are left out, because they would override managed
  settings.
- Codex gets `/etc/codex/config.toml` with `approval_policy = "never"` and
  `sandbox_mode = "danger-full-access"`. `~/.codex/config.toml` overrides any key
  it sets, and `provision.sh` does not set these. `cx` is plain `codex`.
- What still holds, checked in a VM against the installed tools with a stub model
  server that makes each tool run chosen commands: `git push` (plain, by absolute
  path, through `env` and with `-C`) and `git commit --no-verify`, stopped by
  `git_guard.py` in both agents; `gh api` with `-X` or `--method` anywhere, and
  `gh repo delete`, stopped by Claude's deny rules; `gh repo delete`, stopped by
  Codex's `.codex/rules`, which also stop `git push` when its hook is off.
- A gap in Codex: its rules match only the words that start a command.
  `/etc/codex/rules/throwaway.rules` forbids `gh api -X ...` and
  `gh api --method ...`, but `gh api ENDPOINT -X POST` runs, and it is a write
  that the token's scope permits. Claude denies that form.

## Checks to run once with real credentials

After `limavm base` and `limavm new t1`:

- `zsh -ic exit` prints nothing, `git --version` is 2.54 or later, and
  `git hook list pre-commit` lists gitleaks.
- In a fresh repository under `~/src`, `c` opens with no dialogs and shows
  bypass-permissions mode, and `/sandbox` shows that the sandbox is off.
- `gh auth status` accepts the token, and `git push` from the shell works on a
  repository the token covers, while the same push through an agent is refused.
- `claude` runs without a login, and `codex login --device-auth` signs Codex in.
- An LGTMCP `review_only` call works, and `emacs -nw` starts and exits.
- From the VM, `curl -m3 host.lima.internal:PORT` fails at once for a server on
  the Mac, and `lsof -nP -iTCP -sTCP:LISTEN` on the Mac shows only Lima's
  `127.0.0.1` ports.

# Testing on Linux

`make test-linux` needs Docker. It copies the checkout into a temporary
directory outside the repository with `tools/stage_tree.sh`, which includes
files that are not yet committed and scans them with gitleaks. It then builds
`cloudflare/Dockerfile` for the machine's own architecture with that directory
as the context, which runs `./provision.sh` just as on a Linux box, and finally
runs `bun install --frozen-lockfile` and `make test lint` in the image as an
unprivileged user. It fails if any step fails.

`make test-linux MODULES="dev cloud"` provisions other modules (the default is
`dev`, whose tools `make test lint` needs, so keep it in the list), and
`LINUX_IMAGE` names the image (the default is `dotfiles-linux-test`).
