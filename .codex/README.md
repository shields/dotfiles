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
The hook uses `python3` from `PATH` and requires Python 3.14 or later, like the repo.

Prefix rules match literal argument prefixes. The hook supplements those rules
for ordinary shell invocations, command chains, and `sh`/`bash`/`zsh -c` wrappers.
It does not evaluate aliases, dynamically constructed commands, substitutions
inside quoted arguments, or code executed by scripts and interpreters. It skips
here-document bodies. These checks are guardrails, not a complete boundary against
arbitrary code, and do not govern GitHub connector tools.

Run `uv run pytest tests/test_codex_policy.py` to check the hook and rules. Rule
tests use `codex execpolicy check` and skip when the Codex CLI is unavailable;
none of the test commands are executed.

References: [Codex rules](https://learn.chatgpt.com/docs/agent-configuration/rules)
and [Codex hooks](https://learn.chatgpt.com/docs/hooks).
