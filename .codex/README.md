# Codex policy

The rules prohibit pushes, GitHub repository mutations, removing Lefthook, and
committing with leading hook-bypass flags. The Git hook also checks literal shell
commands for pushes with global Git options (including `-C`, `-c`, `--git-dir`,
and `--work-tree`) and commit bypass flags in other positions. Like the Claude
permissions, it rejects commits with Git configuration overrides.

`provision.sh` copies tracked files into `~/.codex/`. After provisioning, restart
Codex and use `/hooks` to review and trust the `git_guard.py` hook. Codex skips new
or changed hook definitions until they are trusted. Hooks are enabled by default;
an explicit `features.hooks = false` setting disables this additional check.

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
`review_and_commit` tools. The script reads the existing TOML to preserve extra
policy, then writes the settings through Codex's `config/batchWrite` app-server
API. Other settings are preserved. Restart Codex after applying these settings.

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

References: [Codex rules](https://learn.chatgpt.com/docs/agent-configuration/rules)
and [Codex hooks](https://learn.chatgpt.com/docs/hooks).
