# Codex policy

The rules prohibit pushes, GitHub repository mutations, removing Lefthook, and
committing with leading hook-bypass flags. The Git hook also checks literal shell
commands for pushes with global Git options (including `-C`, `-c`, `--git-dir`,
and `--work-tree`) and commit bypass flags in other positions, including the
prefixes of `--no-verify` that Git accepts. It reads redirections, line
continuations, unquoted `$(...)` and `<(...)`, and a zsh `=git` as the shell
does, and looks through the wrappers in `WRAPPERS` (`env`, `nice`, `nohup`,
`sudo`, `timeout`, `xargs` and others). It rejects commits with Git configuration
overrides other than `-c` with `user.name`, `user.email`, `commit.gpgsign` or
`tag.gpgsign`, and any `alias.*` override, which can make another word run a
commit. With those four settings it rejects a commit unless every word is a
literal, except that the value of an option may be a double-quoted string that
expands only `$name` or `${name}`; a command substitution or backticks count as
unreadable even inside double quotes. It treats `gh api` as a read only
when every `-X` or `--method`, in any spelling (`-XGET`, `--method=GET`, a
cluster such as `-iX`), names GET or HEAD, and rejects any other method wherever
it stands, which the prefix rules can match only at the start of a command.
Without a method, `-f`, `-F` or `--input` make `gh api` send a POST, so the hook
rejects them too. A GraphQL query is exempt when the command names no method or
names GET or HEAD: `gh api graphql` with a `query` field that is a literal, or a
double-quoted string that starts with `query` or `{`, and that neither reads a
file nor contains `mutation`.

The hook also denies `cf` (or `cloudflare`) commands that change authentication
(`login` and `logout` wherever they appear, which includes the hidden `cf login`
alias, and `auth create`, `delete`, `activate` and `deactivate`), create,
rotate or reveal credentials (any non-read command on a token, secret, API key,
credential or certificate resource, any command on a `token` resource, which
returns the secret itself, `--secrets-file`, which uploads Worker secrets with
a deploy, and verbs such as `create-token`, `revoke-tokens`, `roll`, `rotate`
and `ssh-gen`), change billing, subscriptions, payments and monetization
(`pay-per-crawl`, `pay-per-use`, `monetization`), registrar domains, account or
organization members and invitations, user groups, SSO, SCIM or OAuth clients,
or delete, flush or overwrite anything (`delete`, `bulk-delete`, `purge`,
`purge-cache`, `invalidate`, `restore`, `remove`, `empty`, `disable`, `terminate`
and similar). It checks command names, not the data they carry, so
`cf d1 databases query` with a `DELETE` statement, or `cf kv keys put` over an
existing key, passes. It compares the executable name after Unicode case folding
(`CF`, `ſh` and `cloudﬂare` count as `cf`, `sh` and `cloudflare`), since the
Mac's file system does.

cf accepts a command's own options before, among and after its command words, as
in `cf --scopes x auth login`. The hook skips cf's global options and their
values (`-z`, `--zone`, `--profile`, `-m`, `--mode`, `--persist-to`,
`--event-code`) and checks every other word, the values of other options
included; it assumes that an option it does not know takes the next word as its
value, except for a `--no-` negation. It looks for a command path from the first
word, or from any word in front of it that may be an option's value. It fails
closed when any other word, a positional or an option's value, equals a denied
word, ends in a credential noun (`app-token`, `api-keys`) or has `delete` or
`purge` between hyphens (`cache-purge`), so a read such as
`cf workers scripts get cache-purge` is denied too. An option written with `=`
is not checked, so write `--name=cache-purge`, not `--name cache-purge`; a
positional has no such form, so the user runs that command. A word whose text
the shell settles later (an unquoted `$name`, a command substitution, a brace
expansion, the `{}` of `xargs -I{}`) is denied, not guessed: `"$name"` is
accepted only as the value of `-z`, `--zone` or `--profile` or of an option
written with `=`, and positionals are written literally (`xargs -I %`, not
`-I{}`). A read verb such as `get` or `list` exempts a token, billing or
membership command only when no write verb comes before it and no option could
have taken it as a value, so a name that looks like a verb (`--name list`,
`get-ci`) does not unlock the command. `cf docs`, `cf cli`, `cf schema` and
`cf complete` are exempt because they search, print help, schemas or
completions, or change only cf's own telemetry setting. A bare `--help` or `-h`
is allowed, unless a later spelling of the flag cancels it. cf lets the last one
win, reads `--help=x` and `-h=x` as false for every x but `true`, and also
accepts `--h`, `--no-h` and short groups such as `-qh`, so the hook counts a
`false` right after the flag and every other spelling (`--no-help`, `--h`,
`--no-h`, `--help=x`, `-h=x`, a short group that contains `h`) as a
cancellation; not even `--help=true` requests help. `cf update`, `cf init`,
`cf deploy` and `cf dev` run package managers or project code, which the hook
does not check. Nor does it look through package runners such as `npx cf@latest`
or `pnpm dlx cf`: `.claude/settings.json` denies `npx` and `bunx` for Claude
Code, and Codex has no such rule.

`tests/test_codex_policy.py` checks the word lists against every command in the
installed cf package, with options, option values and positionals before, among
and after the command words, so a new cf release that adds an uncovered shape
fails that test. It expects a verdict from each command's HTTP method and
category, but copies the money and member prefixes and the `auth` verbs from the
hook, so a resource that is missing from both passes. CI installs the version
that `tools/install_ci_tools.sh` pins; bump it by hand.

`provision.sh` copies tracked files into `~/.codex/` and then trusts the hook
definitions it installed, so `/hooks` shows `git_guard.py` as trusted after a
restart of Codex. Codex skips new or changed hook definitions until they are
trusted, so a hand edit to `~/.codex/hooks.json` shows up in `/hooks` as modified
until provisioning runs again or you review it there. Hooks are enabled by
default; an explicit `features.hooks = false` setting disables this additional
check.

The hook command runs `git_guard.py` with the first executable `python3` that
reports Python 3.15 or newer in `/opt/homebrew/bin`, `/usr/local/bin`, and
`/home/linuxbrew/.linuxbrew/bin`, in that order. It never searches `PATH`, so
neither a hijacked `PATH` nor an older Python in `~/.local/bin` can stand in for
it. When none qualifies, the command prints a recovery hint to stderr and exits
2, which Codex treats as a block with the hint as its reason: a missing or
incompatible interpreter stops Bash tool calls rather than silently turning the
check off. Only the lookup fails closed; Codex treats any other
nonzero exit as a non-blocking hook failure and runs the command anyway. Codex
runs the command with `$SHELL -lc` and Claude Code with `sh -c`, so keep it valid
in both.

Claude Code runs the same hook. `.claude/settings.json` registers
`~/.codex/hooks/git_guard.py` as a `PreToolUse` hook for Bash and Monitor (which
runs commands under the Bash permission rules) with the same interpreter lookup,
which also catches the spellings its string-matched deny rules miss, such as
`/usr/bin/git push` and `env git push`. Its registration sets `onFailure` to
`block`, so a crash or a timeout stops the command. Codex has no such option, so
the hook also denies on its own when it runs for 5 seconds. The deny rules for
`gh api` write methods and for `git -c core.hooksPath` stay, because Claude
Code's matcher also looks inside a double-quoted `$(...)` and backticks, which
the hook does not.

Where `/etc/cog-gateway` exists, the machine is a guest of `cog`'s GitHub
gateway: the hook lets `git push` through, provisioning removes
`~/.codex/rules/no-push.rules`, and the gateway decides which branches a push
may update.

Provisioning runs `tools/configure_codex.py` to set `approvals_reviewer = "auto_review"` and
`features.worktrees = true`, and trusts `~/src/github.com/shields/dotfiles` as a
project unless the config already has an entry for that key. Codex matches a
project key against a repository's root exactly, so every other repository still
gets its own trust prompt.

It also configures the CLI status line with the model and reasoning effort,
project, branch, permissions, approval mode, context usage, five-hour and weekly
limits, and fast mode, with status-line colors disabled. These settings are
provisioned on both macOS and Linux, including VM bases.

It selects a `dev` permission profile extending `:workspace`, grants writes to
`~/.cache/uv`, `/Volumes/Data/uv-cache.noindex` and the `config/` subdirectory
of the `cf` CLI's directory, where it saves refreshed OAuth tokens, and enables
network access with the Codex network proxy disabled.
Commands can reach the network directly while filesystem writes remain
sandboxed. Other filesystem and network settings in that profile are preserved;
domain rules are inactive while the proxy is disabled. On machines marked by
`/etc/cog-throwaway`, it selects `:danger-full-access`. `cog` runs the
configuration step again after installing that marker, so the user-level `dev`
selection cannot override the system's throwaway defaults.
Subsequent provisioning keeps the proxy disabled.
After converting an existing machine to a throwaway environment, rerun
`tools/configure_codex.py` and restart Codex.

cf keeps its directory at `~/Library/Preferences/cloudflare` on macOS and
`~/.config/cloudflare` on Linux. cf stores and refreshes its login in `config/`,
and the profile grants writes to that subdirectory of both, on both systems, so
the one that does not exist does nothing. The rest of the directory (profile
bindings, preferences) decides which account an unsandboxed cf acts as, so it
stays read-only. Claude Code's sandbox gets the same grants, and on Linux
`provision.sh` creates `~/.config/cloudflare/config` for the `cloud` module
because that sandbox skips a missing path. A set `XDG_CONFIG_HOME` moves cf's
directory to `$XDG_CONFIG_HOME/cloudflare`, which the grants cover only if it is
`~/.config`.

Machines differ in where the checkouts live. Where `~/src` is
`/Volumes/Data/src` (an external drive), `.zshenv` and `provision.sh` point
`UV_CACHE_DIR` at `/Volumes/Data/uv-cache.noindex`, so that uv can link packages
into venvs on that volume; the `.noindex` suffix keeps Spotlight out of the
cache. On such a machine the script also sets `desktop.git-worktree-root` to
`/Volumes/Data/codex-worktrees`, so that worktrees from `cxw` (the `.zshrc`
alias for `codex --worktree`) share a volume with the cache. Everywhere else, uv
keeps `~/.cache/uv`, Codex keeps its default worktree location, and the write
grants for the missing path, here and in `.claude/settings.json`, do nothing.
The script never removes the worktree setting, so while the drive is unmounted,
`cxw` fails with "cannot create worktree root" until the drive returns or you
delete `[desktop] git-worktree-root` from `~/.codex/config.toml`.

The same run seeds hook trust. The script asks the app server for `hooks/list`,
then writes `hooks.state.<key>.trusted_hash` for every hook that
`~/.codex/hooks.json` defines, using the hash Codex computed for the installed
definition. That re-trusts `git_guard.py` after a change to its command, and Lima
clones and the Cloudflare image inherit the trust. The script fails, showing any
warnings that Codex reports, if Codex lists no hook from that file or says that
the file does not load. With `features.hooks = false` in `config.toml` it skips
this step, since Codex then lists no hooks.

The script reads the existing TOML, then writes the
settings through Codex's `config/batchWrite` app-server API. Other settings are
preserved. The server uses normal configuration loading to support Codex's
compatibility aliases. Restart Codex after applying these settings.

Prefix rules match literal argument prefixes. The hook supplements those rules
for ordinary shell invocations, command chains, and `sh`/`bash`/`zsh -c` wrappers.
It does not evaluate aliases (`home/dot_gitconfig.tmpl` defines `c`, `ca` and
`por`), dynamically constructed commands (an option that an unquoted expansion or
`$'...'` supplies goes unseen when the command names no method or field), Git
configuration from `GIT_CONFIG_*` variables or `git config`, substitutions inside
double quotes or backticks (`TOKEN="$(cf accounts tokens create)"` passes, and
no Claude Code deny rule backs the hook for `cf` as one does for `git` and
`gh`), or code executed by scripts and interpreters. It skips here-document
bodies. These checks are guardrails, not a complete boundary against arbitrary
code, and do not govern GitHub connector tools.

Run `uv run pytest tests/test_codex_policy.py tests/test_agent_hooks.py` to check
the hook, the rules, and how both agents launch the hook. The launch tests run
each registered command with fake interpreters and a decoy `python3` on `PATH`.
Rule tests use `codex execpolicy check` and skip when the Codex CLI is
unavailable; none of the test commands are executed.
`tests/test_configure_codex.py` drives `tools/configure_codex.py` against a stub
`codex` that speaks just enough of the app-server protocol, and checks legacy
settings and permission profiles against the real CLI when installed. All
configuration files stay in temporary test directories, so these tests never
touch `~/.codex`.

References: [Codex permissions](https://learn.chatgpt.com/docs/permissions),
[Codex rules](https://learn.chatgpt.com/docs/agent-configuration/rules)
and [Codex hooks](https://learn.chatgpt.com/docs/hooks).
