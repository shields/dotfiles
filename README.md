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
  zsh shells on Linux export it from there.
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
VM yourself, with a GitHub token that reaches one repository and that you
authorize for it. A VM comes from a base image
that has the same shell, git, Emacs and agent setup as the Mac, so making one
takes about a minute, and `limavm rm` discards it.

```
limavm base [MODULE...]   # once, and again to refresh the base: tens of minutes
limavm new [NAME] [--repo OWNER/REPO | --no-repo] [--no-claude-token] [--no-codex-auth]
                          # a clone of the base, with secrets, the repository, and a shell in it
limavm github NAME [OWNER/REPO] # authorize an existing VM for a repository again
limavm claude-token       # once: keep the Claude token on the Mac for every later new
limavm rm NAME
limavm list
```

`limavm base` must run in a dotfiles checkout, and builds `dotfiles-base` from
`lima/dev.yaml` and that checkout, including files that are not committed
(`tools/stage_tree.sh` copies them). The guest checkout retains the local
branch, commit history and tags; local changes appear as unstaged or untracked
files after provisioning. Gitleaks scans both the copy and its history. It runs
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

No GitHub credential is stored on the Mac for the VMs. Each secret reaches a VM
over stdin, when the VM is made, and exists on the Mac only in `limavm`'s memory
(and in your head, or in a terminal you pasted it into), except for the Claude
token that `limavm claude-token` keeps, and the Codex login and the LGTMCP
config the Mac has anyway, all below.

### GitHub

`limavm new NAME --repo OWNER/REPO` gives the VM access to that one repository,
as you, with push and pull requests, clones it over HTTPS into
`~/src/github.com/OWNER/REPO` in the VM (through the VM's git credential helper,
so no token is in the command) and opens the shell there. The base holds
`shields/dotfiles` there already, with the checkout's local changes, so for that
repository nothing is cloned and the shell opens in that checkout. Run inside a
checkout, `limavm new` takes the repository from the checkout's `origin` when
that is on github.com (`git@`, `ssh://git@` with or without a port, or
`https://`, with or without `.git`), and says so; `--repo` wins over the origin,
and `--no-repo` ignores it. `limavm github NAME [OWNER/REPO]` authorizes a VM
that exists the same way, infers the repository the same way, replaces what the
VM had, and does not clone. Outside a checkout, with no origin or with an origin
elsewhere, a VM made without `--repo` has no GitHub access, and `limavm` says so;
`limavm github` then needs `OWNER/REPO`. A VM reaches one repository: use two VMs
for two.

How it works:

1. The access comes from a GitHub App, which issues user access tokens through
   the device flow. The client ID in `bin/limavm` (`Iv23lim5x4MdkNNgv28z`, which
   is public) is the author's app; to use your own, make the app as follows and
   set `LIMAVM_GITHUB_CLIENT_ID`. You create it once, at github.com/settings/apps:
   Device Flow on, "Expire user authorization tokens" on, no webhook, repository
   permissions Contents and Pull requests set to read and write and nothing else,
   and installed on your account for all repositories. It has no private key, and
   none must ever be made: with a key anyone who held it could mint tokens for
   every repository the app is installed on, and this design needs none.
2. `limavm` looks up the repository's id with your own `gh` login (one read-only
   API call), asks GitHub for a device code, prints it with the address to open,
   puts the code on the clipboard with `pbcopy` and opens the address, and polls
   while you paste the code and authorize. The code is shown on the screen, so
   the clipboard holds nothing secret, and a failing `pbcopy` or `open` just
   says what to do by hand. The token request names the repository by its id
   (`repository_id`), which limits the user access token, and the refresh token
   that comes with it, to that one repository, with push. GitHub keeps that
   limit when the token is refreshed; the design stands on that, so recheck it
   if GitHub changes the device flow.
3. The result goes to `setup-secrets GITHUB_APP_AUTH` in the VM on stdin: an
   access token (`ghu_`, valid 8 hours) and a refresh token (`ghr_`, valid 6
   months, and replaced by a new one every time it is used). It is kept in
   `~/.config/github-app/auth.json` (directory 0700, file 0600, written
   atomically). `setup-secrets` also points git's credential helper for
   `github.com` at `bin/github_app_token.py`, which answers only for that
   repository's path (any other repository gets nothing, and so fails to
   authenticate), and logs `gh` in.
4. The VM renews its own token, with no help from the Mac, which could not be
   reached anyway. `github-app-refresh.timer` (installed by
   `provision/throwaway.sh`, so every clone has it) runs
   `github_app_token.py refresh-if-needed` as the VM user a minute after boot and
   every 5 minutes; it renews when less than 10 minutes remain, and does nothing
   when no `auth.json` exists. The git credential helper renews on demand too.
   Each renewal takes an exclusive file lock and saves the new pair before it
   hands out the new access token, because the old refresh token and access
   token stop working at once. It then logs `gh` in again, so `gh` keeps
   working without action. `gh` keeps the access token (not the refresh token) in
   `~/.config/gh/hosts.yml`, mode 0600.
5. After a long suspension the token is stale for at most 5 minutes: git renews it
   the moment it needs it, `gh` fails until the next timer run, and then works
   again. If the refresh token expired (6 months without a renewal) or GitHub
   rejects it, the command says so and tells you to run
   `limavm github NAME OWNER/REPO`. A rejected token can also mean that a copy of
   it was used elsewhere, since each is good once; the message says that, and
   suggests de-authorizing the app.

Revoking: github.com/settings/apps/authorizations, "Revoke" on the app (listed
under the name you gave it), ends the tokens of every VM at once. GitHub can revoke one token only with the app's
client secret, which this design does not have, so there is no per-VM revocation.
`limavm rm` deletes a VM but does not revoke its tokens: they stop at their
expiry, or when the VM's own refresh fails. To cut off a VM early, de-authorize the
app and run `limavm github NAME OWNER/REPO` for each VM you still use.

Limits: an agent in the VM can read both tokens, and the refresh token lasts
months, so what limits a thief is the one repository and the rotation (a copy that
is used makes the VM's next renewal fail, which is visible), not secrecy. The
repository scope is also the only limit on `gh api` calls that are POSTs without
`-X` (see below).

### Claude Code

`limavm claude-token` asks, without echo, for the token that `claude setup-token`
prints (run that in another terminal first and paste the result) and keeps it in
`~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` on the Mac (directory 0700, file
0600, written atomically). That is the one secret `limavm` itself keeps on the
Mac: the file is the same one the VM gets, `.zshrc` exports it only on Linux,
and Claude's sandbox on the Mac denies reads under `~/.config/secrets`.
`limavm new` installs the token from that file when it exists, and fails, naming
the file, when it is empty, unreadable or holds more than one word; without the
file it asks the same way `claude-token` does and reminds you of `claude-token`.
In the VM the token goes to the same path, which `.zshrc` exports in interactive
shells. It is never taken from the command line or the environment. An empty
answer is an error; `--no-claude-token` skips the token, file or prompt, for a
VM where you will run `claude auth login`.

### The rest

- `~/.config/lgtmcp/config.yaml` on the Mac is copied to the same place in the VM.
- Codex's login on the Mac, `~/.codex/auth.json` (the file OpenAI documents
  copying to a headless machine), goes to `setup-secrets CODEX_AUTH` on stdin,
  which checks that it is a JSON object and writes it to the same path in the VM
  (directory 0700, file 0600). `limavm new` fails before it makes the VM when the
  file is missing, unreadable or not a JSON object: run `codex login` on the Mac
  first. `--no-codex-auth` skips it, and then says to run
  `codex login --device-auth` in the VM, after you turn on device code login in
  ChatGPT's security settings.

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
- The secrets in the VM. The agents there can read the GitHub tokens (the
  record in `~/.config/github-app/` and `gh`'s credentials), the Claude token,
  the Codex login and the LGTMCP key, and send them to any public address. The
  GitHub token's one-repository scope is what limits that, and de-authorizing
  the app ends it.
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
  `git_guard.py` in both agents, which also stops `gh api` with `-X` or
  `--method` anywhere in the command; `gh api` with `-X` or `--method` and
  `gh repo delete`, stopped by Claude's deny rules; `gh repo delete`, stopped by
  Codex's `.codex/rules`, which also stop `git push` when its hook is off.
- What does not stop an agent: `gh api` with `-f`, `-F` or `--input` and no
  `-X` is sent as a POST by default, and neither agent's rules nor the hook
  catches it, because `gh api graphql` queries are read-only POSTs that have to
  keep working. The token's scope is the limit on that.

## Checks to run once with real credentials

After `limavm base` and `limavm new t1 --repo OWNER/REPO`, which shows a code,
puts it on the clipboard and opens the address to paste it at:

- `zsh -ic exit` prints nothing, `git --version` is 2.54 or later, and
  `git hook list pre-commit` lists gitleaks.
- In a fresh repository under `~/src`, `c` opens with no dialogs and shows
  bypass-permissions mode, and `/sandbox` shows that the sandbox is off.
- In the VM `gh auth status` accepts the token, `gh api repos/OWNER/REPO` shows
  push, `git push` and `git pull` from the shell work on that repository, and
  the same push through an agent is refused. A push to a second repository
  fails, and so does `gh api` for it.
- The renewal: `~/bin/github_app_token.py refresh-if-needed --force` there prints
  that it renewed the token and `gh` still works. Waiting 8 hours does the same
  through the timer (`systemctl list-timers github-app-refresh.timer`).
- `limavm github t1 OWNER/REPO` replaces the access in a running VM, and
  de-authorizing the app at github.com/settings/apps/authorizations makes the
  next renewal in every VM fail with a message that names that command.
- `claude` runs without a login (with the token from `limavm claude-token` or
  the `limavm new` prompt), and so does `codex` (with the Mac's `codex login`;
  `codex login status` says so).
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
