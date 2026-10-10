# Codex policy

The rules prohibit pushes, GitHub repository mutations, removing Lefthook, and
committing with leading hook-bypass flags. The Git hook also checks literal shell
commands for pushes with global Git options (including `-C`, `-c`, `--git-dir`,
and `--work-tree`) and commit bypass flags in other positions. Like the Claude
permissions, it rejects commits with Git configuration overrides. It also rejects
`gh api` with `-X` or `--method` anywhere in the command, in any spelling
(`-XPOST`, `--method=PUT`, a cluster such as `-iX`), which the prefix rules can
match only at the start of a command. A POST that `gh api` sends by default
because of `-f`, `-F` or `--input` is not rejected, since GraphQL queries need
that.

`provision.sh` copies tracked files into `~/.codex/` and then trusts the hook
definitions it installed, so `/hooks` shows `git_guard.py` as trusted after a
restart of Codex. Codex skips new or changed hook definitions until they are
trusted, so a hand edit to `~/.codex/hooks.json` shows up in `/hooks` as modified
until provisioning runs again or you review it there. Hooks are enabled by
default; an explicit `features.hooks = false` setting disables this additional
check.

The hook command runs `git_guard.py` with the first `python3.14` that exists in
`/opt/homebrew/bin`, `/usr/local/bin`, and `/home/linuxbrew/.linuxbrew/bin`, in
that order. It never searches `PATH`, so neither a hijacked `PATH` nor an older
Python in `~/.local/bin` can stand in for it. When none exists, the command prints
a recovery hint to stderr and exits 2, which Codex treats as a block with the hint
as its reason: a missing interpreter stops Bash tool calls rather than silently
turning the check off. Only the lookup fails closed; Codex treats any other
nonzero exit as a non-blocking hook failure and runs the command anyway. Codex
runs the command with `$SHELL -lc` and Claude Code with `sh -c`, so keep it valid
in both.

Claude Code runs the same hook. `.claude/settings.json` registers
`~/.codex/hooks/git_guard.py` as a `PreToolUse` hook for Bash with the same
interpreter lookup, which also catches the spellings its string-matched deny rules
miss, such as `/usr/bin/git push` and `env git push`.

Provisioning runs `tools/configure_codex.py` to authorize LGTMCP's code transfers to
Gemini in `auto_review.extra_policy` and preapprove its `review_only` and
`review_and_commit` tools. It also sets `approvals_reviewer = "auto_review"` and
`features.worktrees = true`, and trusts `~/src/github.com/shields/dotfiles` as a
project unless the config already has an entry for that key. Codex matches a
project key against a repository's root exactly, so every other repository still
gets its own trust prompt.

It selects a `dev` permission profile extending `:workspace`, grants writes to
`~/.cache/uv`, and enables the network proxy with localhost and `127.0.0.1`
allowed. Local binding is enabled so tests can start local servers. Other
filesystem and network rules in that profile are preserved. Older `sandbox_mode`
settings take precedence over permission profiles, including the full-access
settings in throwaway environments.

The same run seeds hook trust. The script asks the app server for `hooks/list`,
then writes `hooks.state.<key>.trusted_hash` for every hook that
`~/.codex/hooks.json` defines, using the hash Codex computed for the installed
definition. That re-trusts `git_guard.py` after a change to its command, and Lima
clones and the Cloudflare image inherit the trust. The script fails, showing any
warnings that Codex reports, if Codex lists no hook from that file or says that
the file does not load. With `features.hooks = false` in `config.toml` it skips
this step, since Codex then lists no hooks.

The script reads the existing TOML to preserve extra policy, then writes the
settings through Codex's `config/batchWrite` app-server API. Other settings are
preserved. The server uses normal configuration loading to support Codex's
compatibility aliases. Restart Codex after applying these settings.

Prefix rules match literal argument prefixes. The hook supplements those rules
for ordinary shell invocations, command chains, and `sh`/`bash`/`zsh -c` wrappers.
It does not evaluate aliases, dynamically constructed commands, substitutions
inside quoted arguments, or code executed by scripts and interpreters. It skips
here-document bodies. These checks are guardrails, not a complete boundary against
arbitrary code, and do not govern GitHub connector tools.

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
