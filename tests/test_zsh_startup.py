#!/usr/bin/env python3
"""Exercise startup ordering in real interactive Zsh sessions with small plugins."""

# Copyright © 2026 Michael Shields
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import pty
import select
import shlex
import shutil
import signal
import subprocess
import tempfile
import time
import unittest
from contextlib import suppress
from pathlib import Path
from typing import final, override

# Use the standard library test runner, like the other standalone test scripts.
# ruff: noqa: PT009
# unittest initializes fixtures in setUp, before each test and its cleanups.
# pyright: reportUninitializedInstanceVariable=false

REPO = Path(__file__).resolve().parents[1]
ZSH = shutil.which("zsh") or ""
DEFER = Path(
    os.environ.get("ZSH_DEFER_DIR", str(Path.home() / ".local/share/zsh-defer"))
)
HAS_DEFER = (DEFER / "zsh-defer.plugin.zsh").is_file()


@unittest.skipUnless(ZSH, "zsh is required")
@final
class StartupTest(unittest.TestCase):
    @override
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="test-zsh-startup-")
        self.addCleanup(self.temporary.cleanup)
        self.test_home = Path(self.temporary.name).resolve()
        self.events = self.test_home / "events"
        self.output = bytearray()
        self.pid = 0
        self.fd = -1
        self.addCleanup(self.close_shell)
        self.env = dict(os.environ)
        self.env.update(
            HOME=str(self.test_home),
            ZDOTDIR=str(self.test_home),
            XDG_CACHE_HOME=str(self.test_home / "cache"),
            ZSH_DEFER_DIR=str(DEFER),
            TERM="xterm-256color",
            TERM_PROGRAM="startup-test",
        )
        for name in ("ZSH_CUSTOM", "ZSH_CACHE_DIR", "ZSH_COMPDUMP", "STARSHIP_CONFIG"):
            _ = self.env.pop(name, None)
        self.write(".zshenv", 'HISTFILE="$ZDOTDIR/history"\n')
        (self.test_home / ".zsh.d").symlink_to(
            REPO / ".zsh.d", target_is_directory=True
        )
        self.write(
            ".oh-my-zsh/lib/base.zsh",
            """
setopt auto_cd auto_pushd share_history prompt_subst
bindkey -e
compdef _files 'command with spaces'
""",
        )
        plugins = {
            "direnv": "export STARTUP_TEST_ENV=ready",
            "gcloud": "compdef _files cloud-test",
            "starship": "PROMPT='startup> '; RPROMPT='unused'",
            "aws": "compdef _files aws-test",
            # Inject a command between queued plugins, without depending on
            # wall-clock timing or the speed of the machine running the test.
            "colorize": """
if [[ -n ${zsh_defer_options+x} ]]; then
    {
        print colorize-waiting >> "$ZDOTDIR/events"
        while [[ ! -f "$ZDOTDIR/continue" ]]; do sleep 0.01; done
    } always {
        print colorize-unwound >> "$ZDOTDIR/events"
    }
fi
""",
            "docker": "compdef _files docker-test",
            "emacs": ":",
            "fzf": """
_test_fzf() { :; }
zle -N fzf-completion _test_fzf
bindkey '^I' fzf-completion
""",
            "fzf-tab": """
[[ $(bindkey '^I') == *' fzf-completion' ]] || print BAD-FZF-ORDER >> "$ZDOTDIR/events"
zle -A fzf-completion fzf-tab-complete
bindkey '^I' fzf-tab-complete
""",
            "git": (
                "alias gc='git commit'; alias gcl='git clone'; compdef _files git-test"
            ),
            "git-auto-fetch": 'git-fetch-all() { print FETCH >> "$ZDOTDIR/events"; }',
            "git-prompt-watcher": """
_stop_git_watcher() { :; }
_check_git_repo_change() { :; }
_git_prompt_watcher_exit() { :; }
chpwd_functions+=(_check_git_repo_change)
zshexit_functions+=(_git_prompt_watcher_exit)
TRAPUSR1() { print SIGNAL >> "$ZDOTDIR/events"; }
""",
            "kubectl": "compdef _files kubectl-test",
            "zoxide": "j() { :; }",
        }
        for name, body in plugins.items():
            self.write(
                f".oh-my-zsh/plugins/{name}/{name}.plugin.zsh",
                f'print load:{name} >> "$ZDOTDIR/events"\n{body}\n',
            )
        self.write(
            ".zshrc",
            f"""
source {shlex.quote(str(REPO / ".zshrc"))}
_test_snapshot() {{
    local -A snapshot_comps
    (( ${{+_comps}} )) && snapshot_comps=("${{(@kv)_comps}}")
    local -a state=(
        "$1" "pending=${{_startup_pending:-0}}" "env=${{STARTUP_TEST_ENV-}}"
        "gc=${{aliases[gc]-}}" "gcl_alias=${{+aliases[gcl]}}"
        "gcl=${{+functions[gcl]}}" "md_alias=${{+aliases[md]}}"
        "md=${{+functions[md]}}" "wt=${{snapshot_comps[wt]-}}"
        "space=${{snapshot_comps[command with spaces]-}}"
        "cloud=${{snapshot_comps[cloud-test]-}}"
        "tab=$(bindkey '^I')" "trap=${{+functions[TRAPUSR1]}}"
        "j=${{+functions[j]}}"
    )
    print -r -- "${{(j: :)state}}" >> "$ZDOTDIR/events"
}}
_test_done() {{ _test_snapshot done; }}
if (( ${{_startup_pending:-0}} )); then
    zsh-defer -a _test_done
else
    _test_done
fi
""",
        )

    def write(self, relative: str, content: str) -> None:
        target = self.test_home / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        _ = target.write_text(content)

    def close_shell(self) -> None:
        if self.pid:
            with suppress(ProcessLookupError):
                os.kill(self.pid, signal.SIGKILL)
            _ = os.waitpid(self.pid, 0)
            self.pid = 0
        if self.fd >= 0:
            os.close(self.fd)
            self.fd = -1

    def start_shell(self, first_command: str = "_test_snapshot first\n") -> None:
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.chdir(self.test_home)
            os.execve(ZSH, [ZSH, "-i"], self.env)  # noqa: S606 -- test shell
        _ = os.write(self.fd, first_command.encode())

    def wait_for(self, prefix: str, count: int = 1) -> str:
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            if select.select([self.fd], [], [], 0.02)[0]:
                try:
                    self.output.extend(os.read(self.fd, 65536))
                except OSError:
                    break
            records = self.events.read_text() if self.events.exists() else ""
            if sum(line.startswith(prefix) for line in records.splitlines()) >= count:
                return records
        output = self.output[:4000].decode(errors="replace")
        message = f"No {prefix!r} event; output: {output}"
        raise AssertionError(message)

    def assert_complete(self, record: str) -> None:
        for expected in (
            "pending=0",
            "env=ready",
            "gc=gcloud",
            "gcl_alias=0",
            "gcl=1",
            "md_alias=0",
            "md=1",
            "wt=_wt",
            "space=_files",
            "cloud=_files",
            'tab="^I" fzf-tab-complete',
            "trap=1",
            "j=1",
        ):
            self.assertIn(expected, record)

    @unittest.skipUnless(HAS_DEFER, "install zsh-defer or set ZSH_DEFER_DIR")
    def test_commands_run_before_and_between_plugin_loads(self) -> None:
        self.start_shell()
        _ = self.wait_for("first ")
        _ = self.wait_for("colorize-waiting")
        _ = os.write(self.fd, b"_test_snapshot midway\n")
        (self.test_home / "continue").touch()
        _ = self.wait_for("done ")
        records = self.wait_for("midway ").splitlines()
        first = next(line for line in records if line.startswith("first "))
        midway = next(line for line in records if line.startswith("midway "))
        self.assertIn("pending=1", first)
        self.assertIn("env=ready", first)
        self.assertIn("gc=gcloud", first)
        self.assertLess(records.index(first), records.index("load:aws"))
        self.assertLess(records.index("load:colorize"), records.index(midway))
        self.assertLess(records.index(midway), records.index("load:docker"), records)
        self.assertIn("pending=1", midway)
        self.assertNotIn("BAD-FZF-ORDER", records)
        self.assertEqual(records.count("FETCH"), 1)
        self.assert_complete(next(line for line in records if line.startswith("done ")))
        # Check the trap after returning from the deferred loader's scope.
        _ = os.write(self.fd, b"kill -USR1 $$; _test_snapshot after\n")
        records = self.wait_for("after ")
        self.assertIn("SIGNAL\n", records)
        self.assert_complete(
            next(line for line in records.splitlines() if line.startswith("after "))
        )

    @unittest.skipUnless(HAS_DEFER, "install zsh-defer or set ZSH_DEFER_DIR")
    def test_resourcing_pending_and_completed_startup(self) -> None:
        (self.test_home / "continue").touch()
        self.start_shell('source "$ZDOTDIR/.zshrc"\n')
        records = self.wait_for("done ", 2)
        self.assertEqual(records.splitlines().count("load:git"), 1)
        _ = os.write(self.fd, b'source "$ZDOTDIR/.zshrc"\n')
        records = self.wait_for("done ", 3)
        self.assertEqual(records.splitlines().count("load:git"), 2)
        self.assert_complete(
            [line for line in records.splitlines() if line.startswith("done ")][-1]
        )
        command = "print hooks:${(M)chpwd_functions:#_check_git_repo_change}"
        command += ' >> "$ZDOTDIR/events"\n'
        _ = os.write(self.fd, command.encode())
        records = self.wait_for("hooks:")
        self.assertIn("hooks:_check_git_repo_change\n", records)

    @unittest.skipUnless(HAS_DEFER, "install zsh-defer or set ZSH_DEFER_DIR")
    def test_resourcing_recovers_interrupted_startup(self) -> None:
        self.start_shell()
        _ = self.wait_for("colorize-waiting")
        # Interrupt a task after the scheduler has removed its fd handler.
        # Send SIGINT directly, independent of ZLE's terminal signal mode.
        os.kill(self.pid, signal.SIGINT)
        _ = self.wait_for("colorize-unwound")
        # Finish discarding the interrupted line before the next command.
        _ = os.write(self.fd, b"\n_test_snapshot interrupted\n")
        records = self.wait_for("interrupted ")
        interrupted = next(
            line for line in records.splitlines() if line.startswith("interrupted ")
        )
        self.assertIn("pending=1", interrupted)
        self.assertNotIn("load:docker\n", records)
        (self.test_home / "continue").touch()
        _ = os.write(self.fd, b'source "$ZDOTDIR/.zshrc"\n')
        records = self.wait_for("done ", 2).splitlines()
        self.assert_complete(next(line for line in records if line.startswith("done ")))
        # Retry the interrupted task without replaying completed plugins or
        # duplicating the remaining queue.
        self.assertEqual(records.count("load:aws"), 1)
        self.assertEqual(records.count("load:colorize"), 2)
        self.assertEqual(records.count("load:docker"), 1)
        self.assertEqual(records.count("load:git"), 1)

    def run_command(self, command: str) -> str:
        result = subprocess.run(
            [ZSH, "-ic", command],
            env=self.env,
            cwd=self.test_home,
            capture_output=True,
            text=True,
            timeout=15,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout.strip()

    def test_omz_revision_invalidates_completion_registrations(self) -> None:
        omz = self.test_home / ".oh-my-zsh"

        def git(*args: str) -> None:
            _ = subprocess.run(
                [
                    "git",
                    "-C",
                    str(omz),
                    "-c",
                    "user.name=Startup Test",
                    "-c",
                    "user.email=startup@example.invalid",
                    "-c",
                    "commit.gpgsign=false",
                    "-c",
                    "core.hooksPath=/dev/null",
                    *args,
                ],
                env=self.env,
                capture_output=True,
                text=True,
                check=True,
            )

        self.write(".oh-my-zsh/completions/_old_test", "#compdef update-test\n")
        git("init", "-q")
        git("add", ".")
        git("commit", "-qm", "Initial completion")
        query = 'print -r -- "${_comps[update-test]-missing}"'
        self.assertEqual(self.run_command(query), "_old_test")
        dump = next(self.test_home.glob(".zcompdump-*"))
        initial_mtime = dump.stat().st_mtime_ns
        self.assertEqual(self.run_command(query), "_old_test")
        self.assertEqual(dump.stat().st_mtime_ns, initial_mtime)

        # Both the fpath and completion file count remain unchanged.
        _ = (omz / "completions/_old_test").rename(omz / "completions/_new_test")
        git("add", "-A")
        git("commit", "-qm", "Rename completion function")
        self.assertEqual(self.run_command(query), "_new_test")

        self.write(".oh-my-zsh/completions/_new_test", "#compdef renamed-test\n")
        git("add", "-A")
        git("commit", "-qm", "Change completion registration")
        self.assertEqual(self.run_command(query), "missing")
        query = 'print -r -- "${_comps[renamed-test]-missing}"'
        self.assertEqual(self.run_command(query), "_new_test")

    def test_completion_security_handler_follows_compinit_audit(self) -> None:
        self.write(
            ".oh-my-zsh/lib/compfix.zsh",
            'handle_completion_insecurities() { print compfix >> "$ZDOTDIR/events"; }',
        )
        self.write(".oh-my-zsh/custom/functions/_audit_test", "#compdef audit-test\n")
        completion_dir = self.test_home / ".oh-my-zsh/custom/functions"
        completion_dir.chmod(0o777)  # Intentionally insecure fixture.
        query = 'print -r -- "${_comps[audit-test]-missing}"'
        self.assertEqual(self.run_command(query), "missing")
        self.assertEqual(self.events.read_text().splitlines().count("compfix"), 1)

        # Fix permissions and re-source in the same shell, so the flag from
        # the first audit must be reset before running the second one.
        self.events.unlink()
        command = f"chmod 755 {shlex.quote(str(completion_dir))}; "
        command += 'source "$ZDOTDIR/.zshrc"; ' + query
        self.assertEqual(self.run_command(command), "_audit_test")
        self.assertEqual(self.events.read_text().splitlines().count("compfix"), 1)

    def test_command_string_initializes_synchronously(self) -> None:
        _ = self.run_command("_test_snapshot command")
        records = self.events.read_text().splitlines()
        self.assert_complete(
            next(line for line in records if line.startswith("command "))
        )
        self.assertNotIn("BAD-FZF-ORDER", records)

    def test_missing_scheduler_falls_back_to_synchronous_startup(self) -> None:
        self.env["ZSH_DEFER_DIR"] = str(self.test_home / "missing-defer")
        self.start_shell()
        records = self.wait_for("first ").splitlines()
        self.assert_complete(
            next(line for line in records if line.startswith("first "))
        )


if __name__ == "__main__":
    _ = unittest.main()
