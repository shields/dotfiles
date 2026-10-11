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

Provisioning upgrades formulae through Bundle, which also manages services
declared with `restart_service: :changed` in a Brewfile. Cask upgrades run
separately and exclude Chrome. After the first signed bottle is published, add
`shields/tap/mute-on-lock` to the macOS Brewfile with
`restart_service: :changed` to start its login service and restart it on upgrades.

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
the `git_guard.py` hook, the GitHub gateway and the network isolation below.
A VM comes from a base image
that has the same shell, git, Emacs and agent setup as the Mac, so making one
takes about a minute, and `limavm rm` discards it.

```
limavm base [MODULE...]   # once, and again to refresh the base: tens of minutes
limavm new [NAME] [--repo OWNER/REPO | --no-repo] [--no-claude-token] [--no-codex-auth]
                          # a clone of the base, with secrets, the repository, and a shell in it
limavm github NAME [OWNER/REPO] # add a repository grant to an existing VM
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

## GitHub access

Run `./provision.sh dev` on the Mac to build and start the `vm-access` launchd
service. It holds the GitHub App access and refresh tokens under
`~/.config/secrets/vm-access/`. A Lima guest receives only a public CA
certificate and an immutable VM ID, never a GitHub token. Its HTTPS requests
for `github.com` and `api.github.com` go through a per-VM Unix socket to the
Mac. The gateway chooses the repository's token after checking the current
grant and request. It refreshes tokens on demand; a refresh failure denies the
request. The VM's git credential helper and `GH_TOKEN` use a fixed dummy value
that the gateway discards.

`limavm new NAME --repo OWNER/REPO` grants repository reads, including pull
request reads, and pushes to `task/VM-ID/*`. It then clones the repository over
HTTPS into `~/src/github.com/OWNER/REPO` and opens a shell there. The base
already holds `shields/dotfiles` with the checkout's local changes, so it
reuses that checkout. With no explicit repository, `limavm new` infers one
from a GitHub `origin`; `--no-repo` creates a VM with no grants. `limavm github
NAME [OWNER/REPO]` adds another repository without stopping the VM or removing
its existing grants. It infers the repository from the current checkout if the
argument is omitted.

The gateway accepts Git fetches and pushes to the VM's task branch namespace,
including force pushes. It checks every ref update before forwarding a push;
deletions and push options are denied. GitHub REST GET requests and GraphQL
queries for exactly one granted repository are allowed. The `/user` identity
read uses a granted repository token. GitHub API writes, PR creation and
updates, cross-repository GraphQL queries, search queries and other unlisted
operations are denied. `gh pr list` with search filters may therefore fail;
basic repository and PR reads work through the supported routes. There is no
approval queue.

The version-controlled profiles in `.config/vm-access/profiles.yaml` define
capability templates. A grant stores a copy of its profile, so editing the
profile changes future grants only. Use these commands on the Mac to change a
running VM immediately:

```
vm-access show lima:NAME
vm-access grant lima:NAME OWNER/REPO --profile read|task
vm-access revoke lima:NAME OWNER/REPO
vm-access edit lima:NAME
vm-access logs lima:NAME [--follow|--all]
```

`edit` opens the full YAML policy in `$VISUAL`, `$EDITOR` or `vi`; it rejects a
stale revision. Adding a repository runs the GitHub authorization on the Mac
before applying the edit. `grant` also authorizes a repository if needed. The
gateway reads grants for every request, so changes take effect without a VM
restart. `limavm rm` disables access before deleting the VM. A new VM with the
same display name receives a new ID and no old grants. `vm-access logs VM-ID`
can inspect a deleted VM's retained history.

The GitHub App uses device flow with expiring user tokens and no private key.
For your own app, grant repository Contents read/write and Pull requests read;
the gateway denies PR writes even if an app has broader permissions. The
default public client ID is `Iv23lim5x4MdkNNgv28z`; set
`LIMAVM_GITHUB_CLIENT_ID` for your own app. The Mac's `gh` login reads the
repository ID; the device flow requests that `repository_id` to scope the app
token, and the gateway verifies that token can read the intended repository.
The device code and URL are printed on the Mac for you to complete. To replace
an expired or revoked authorization, run `vm-access authorize OWNER/REPO` on
the Mac; policy grants remain in place. Revoking the app authorization at
GitHub invalidates all its tokens. The gateway keeps the rotating refresh
tokens on the Mac and never sends them to a VM.

Every request decision is logged with VM ID, repository, operation, permitted
refs, policy revision, result, upstream status and duration. Headers, bodies
and raw queries are not logged. Policy changes are separately audited with the
actor and old/new revision. Request logs are kept for 30 days and policy
events for one year; requests fail closed if the audit database cannot be
written. The state and admin socket are under `~/.local/state/vm-access/` on
the Mac. `vm-access logs lima:NAME --follow` streams recent entries, and
`--all` prints the retained history.

## Other secrets

### Claude Code

`limavm claude-token` asks, without echo, for the token that `claude setup-token`
prints (run that in another terminal first and paste the result) and keeps it in
`~/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN` on the Mac (directory 0700, file
0600, written atomically). That is the one secret `limavm` itself keeps on the
Mac: the file is the same one the VM gets, `.zshrc` exports it only on Linux,
and Claude's sandbox on the Mac denies reads under `~/.config/secrets`.
`limavm new` installs the token from that file when it exists, and fails, naming
the file, when it is not a regular file, empty, unreadable or holds more than
one word; without the file it asks the same way `claude-token` does and reminds
you of `claude-token`.
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
  file is missing, not a regular file, unreadable, empty or not a JSON object.
  The file exists only when `cli_auth_credentials_store` in
  `~/.codex/config.toml` is `file`: with `keyring` or `auto` Codex keeps the
  login in the Keychain, and OpenAI says the copy does not apply then. So set
  `cli_auth_credentials_store = "file"` and run `codex login` on the Mac first.
  `--no-codex-auth` skips it, and then says to run `codex login --device-auth`
  in the VM, after you turn on device code login in ChatGPT's security
  settings.

## What is isolated

Isolated, with `lima/dev.yaml` as the source:

- No host directory is mounted. `lima/dev.yaml` sets `mounts: []`, and
  `limavm base` fails if `~/.lima/_config` has added any.
- No guest port is forwarded to the Mac. Lima still opens `127.0.0.1` ports on the
  Mac for SSH and for its DNS resolver, which other programs on the Mac can
  reach. The GitHub gateway has one reverse Unix socket forward per VM.
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
  delete the filter or the GitHub hosts mapping. These controls stop ordinary
  clients, not a determined attacker.
- Direct connections to GitHub IPs, other GitHub hostnames or a new outbound
  proxy. General internet access is unrestricted, so the gateway cannot force
  every possible GitHub connection through itself. It does keep its app tokens
  off the VM; the policy is a boundary around those tokens, not around arbitrary
  network traffic or credentials acquired by the guest.
- The Claude token, Codex login and LGTMCP key in the VM. Agents can read and
  send those to public addresses. The VM has no GitHub App token or refresh
  token.
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
- In a gateway VM, `git_guard.py` and the installed deny rules allow `git push`;
  the gateway checks the destinations against `task/VM-ID/*`. The hook still
  stops `git commit --no-verify`, and the agent policies still refuse the
  listed `gh` write commands. The gateway is the final check for API calls,
  including POST requests formed by `gh api -f`, `-F` or `--input` without
  `-X`.

## Checks to run once with real credentials

After `./provision.sh dev`, `limavm base` and `limavm new t1 --repo
OWNER/REPO`, which shows a device code and authorization URL on the Mac:

- `zsh -ic exit` prints nothing, `git --version` is 2.54 or later, and
  `git hook list pre-commit` lists gitleaks.
- In a fresh repository under `~/src`, `c` opens with no dialogs and shows
  bypass-permissions mode, and `/sandbox` shows that the sandbox is off.
- In the VM, `gh api repos/OWNER/REPO` and `git pull` work. A push to
  `task/VM-ID/check` succeeds, including a forced update. A push to `main`, a
  branch deletion, a PR write, and a read of an ungranted private repository
  fail. `vm-access logs lima:t1` on the Mac records each decision.
- `limavm github t1 OTHER/REPO` adds a grant while the VM runs; revoking it with
  `vm-access revoke lima:t1 OTHER/REPO` takes effect on the next request.
- Replacing an expiring access token happens on the Mac on demand. After a
  revoked or expired refresh token, `vm-access authorize OWNER/REPO` restores
  the authorization; the VM never receives it.
- `claude` runs without a login (with the token from `limavm claude-token` or
  the `limavm new` prompt), and so does `codex` (with the Mac's `codex login`;
  `codex login status` says so).
- An LGTMCP `review_only` call works, and `emacs -nw` starts and exits.
- From the VM, `curl -m3 host.lima.internal:PORT` fails at once for a server on
  the Mac, and `lsof -nP -iTCP -sTCP:LISTEN` on the Mac shows only Lima's
  `127.0.0.1` ports.

# Throwaway Cloudflare VMs

`cloudflare/deploy.sh` stages the checkout, scans it for secrets, builds the
agent image, deploys the Worker, and installs a randomly generated admin secret
from `~/.config/secrets/vm-access/cloudflare-admin-token`. It adds this Mac's
`~/.ssh/id_ed25519.pub` to the staged Wrangler configuration for SSH access.
The Worker URL appears in Wrangler's deployment output. Save it once with
`bun run cloudflare/admin.ts config https://NAME.SUBDOMAIN.workers.dev`.
For Worker-only changes, `cloudflare/deploy.sh --worker-only` reuses the
deployed image without building or rolling out containers.

Use `bun run cloudflare/admin.ts` for management:

```sh
bun run cloudflare/admin.ts authorize OWNER/REPO
bun run cloudflare/admin.ts new NAME
bun run cloudflare/admin.ts grant VM-ID OWNER/REPO task
bun run cloudflare/admin.ts start VM-ID
bun run cloudflare/admin.ts show VM-ID
bun run cloudflare/admin.ts logs VM-ID
bun run cloudflare/admin.ts events VM-ID
bun run cloudflare/admin.ts revoke VM-ID OWNER/REPO
bun run cloudflare/admin.ts delete VM-ID
```

`authorize` starts a separate GitHub App device flow for the cloud authority;
it does not save that authorization on the Mac. A grant uses the same `read`
and `task` profiles as Lima, and a revision check prevents overwriting another
policy edit. Grants and revocations take effect for the next GitHub request
without restarting the VM. `list` shows registered VM IDs. `containers`,
`instances APPLICATION-ID`, and `ssh CONTAINER-ID` wrap the corresponding
Wrangler commands. After connecting, clone a granted repository with Git and
log in to the agents in the disposable VM as needed.

The VM receives only a dummy GitHub credential. Its outbound HTTP and HTTPS
requests pass through the Worker, which checks GitHub operations against the
current VM grants and supplies the repository's short-lived token. The refresh
token stays in Durable Object storage. GitHub access logs record the VM,
repository, operation, refs, decision, policy revision, upstream status, and
duration without credentials; policy events record before and after grants.
Access logs are retained for 30 days and policy events for 365 days. The
container's filesystem is disposable. The current image uses a 1-vCPU,
4-GiB, 20-GB instance to leave workspace room around the installed toolchain;
its inactivity timeout is six hours.

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
