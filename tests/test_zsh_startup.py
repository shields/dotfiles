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
import time
from contextlib import suppress
from pathlib import Path
from typing import TYPE_CHECKING, final

import pytest

if TYPE_CHECKING:
    from collections.abc import Iterator

REPO = Path(__file__).resolve().parents[1]
ZSH = shutil.which("zsh") or ""
DEFER = Path(
    os.environ.get("ZSH_DEFER_DIR", str(Path.home() / ".local/share/zsh-defer"))
)
HAS_DEFER = (DEFER / "zsh-defer.plugin.zsh").is_file()

pytestmark = pytest.mark.skipif(not ZSH, reason="zsh is required")
needs_defer = pytest.mark.skipif(
    not HAS_DEFER, reason="install zsh-defer or set ZSH_DEFER_DIR"
)

BASE_ZSH = """
setopt auto_cd auto_pushd share_history prompt_subst
bindkey -e
compdef _files 'command with spaces'
"""

PLUGINS = {
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
    "git": "alias gc='git commit'; alias gcl='git clone'; compdef _files git-test",
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

ZSHRC = f"""
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
"""


@final
class Shell:
    """A home with a fake oh-my-zsh, and the interactive zsh started in it."""

    def __init__(self, home: Path) -> None:
        self.home = home
        self.events = home / "events"
        self.output = bytearray()
        self.pid = 0
        self.fd = -1
        self.env = dict(os.environ)
        self.env.update(
            HOME=str(home),
            ZDOTDIR=str(home),
            XDG_CACHE_HOME=str(home / "cache"),
            ZSH_DEFER_DIR=str(DEFER),
            TERM="xterm-256color",
            TERM_PROGRAM="startup-test",
        )
        for name in ("ZSH_CUSTOM", "ZSH_CACHE_DIR", "ZSH_COMPDUMP", "STARSHIP_CONFIG"):
            _ = self.env.pop(name, None)
        self.write(".zshenv", 'HISTFILE="$ZDOTDIR/history"\n')
        (home / ".zsh.d").symlink_to(REPO / ".zsh.d", target_is_directory=True)
        self.write(".oh-my-zsh/lib/base.zsh", BASE_ZSH)
        for name, body in PLUGINS.items():
            self.write(
                f".oh-my-zsh/plugins/{name}/{name}.plugin.zsh",
                f'print load:{name} >> "$ZDOTDIR/events"\n{body}\n',
            )
        self.write(".zshrc", ZSHRC)

    def write(self, relative: str, content: str) -> None:
        target = self.home / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        _ = target.write_text(content)

    def close(self) -> None:
        if self.pid:
            with suppress(ProcessLookupError):
                os.kill(self.pid, signal.SIGKILL)
        # Release the master before reaping: a killed shell can stay stuck
        # exiting for as long as the pty master is open, and so would waitpid.
        if self.fd >= 0:
            os.close(self.fd)
            self.fd = -1
        if self.pid:
            _ = os.waitpid(self.pid, 0)
            self.pid = 0

    def start(self, first_command: str = "_test_snapshot first\n") -> None:
        self.pid, self.fd = pty.fork()
        if self.pid == 0:
            os.chdir(self.home)
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

    def run_command(self, command: str) -> str:
        result = subprocess.run(
            [ZSH, "-ic", command],
            env=self.env,
            cwd=self.home,
            capture_output=True,
            text=True,
            timeout=15,
            check=False,
        )
        assert result.returncode == 0, result.stderr
        return result.stdout.strip()

    def git(self, *args: str) -> None:
        _ = subprocess.run(
            [
                "git",
                "-C",
                str(self.home / ".oh-my-zsh"),
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


def assert_complete(record: str) -> None:
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
        assert expected in record


@pytest.fixture
def shell(tmp_path: Path) -> Iterator[Shell]:
    shell = Shell(tmp_path.resolve())
    yield shell
    shell.close()


@needs_defer
def test_commands_run_before_and_between_plugin_loads(shell: Shell) -> None:
    shell.start()
    _ = shell.wait_for("first ")
    _ = shell.wait_for("colorize-waiting")
    _ = os.write(shell.fd, b"_test_snapshot midway\n")
    (shell.home / "continue").touch()
    _ = shell.wait_for("done ")
    records = shell.wait_for("midway ").splitlines()
    first = next(line for line in records if line.startswith("first "))
    midway = next(line for line in records if line.startswith("midway "))
    assert "pending=1" in first
    assert "env=ready" in first
    assert "gc=gcloud" in first
    assert records.index(first) < records.index("load:aws")
    assert records.index("load:colorize") < records.index(midway)
    assert records.index(midway) < records.index("load:docker"), records
    assert "pending=1" in midway
    assert "BAD-FZF-ORDER" not in records
    assert records.count("FETCH") == 1
    assert_complete(next(line for line in records if line.startswith("done ")))
    # Check the trap after returning from the deferred loader's scope.
    _ = os.write(shell.fd, b"kill -USR1 $$; _test_snapshot after\n")
    later = shell.wait_for("after ")
    assert "SIGNAL\n" in later
    assert_complete(
        next(line for line in later.splitlines() if line.startswith("after "))
    )


@needs_defer
def test_resourcing_pending_and_completed_startup(shell: Shell) -> None:
    (shell.home / "continue").touch()
    shell.start('source "$ZDOTDIR/.zshrc"\n')
    records = shell.wait_for("done ", 2)
    assert records.splitlines().count("load:git") == 1
    _ = os.write(shell.fd, b'source "$ZDOTDIR/.zshrc"\n')
    records = shell.wait_for("done ", 3)
    assert records.splitlines().count("load:git") == 2
    assert_complete(
        [line for line in records.splitlines() if line.startswith("done ")][-1]
    )
    command = "print hooks:${(M)chpwd_functions:#_check_git_repo_change}"
    command += ' >> "$ZDOTDIR/events"\n'
    _ = os.write(shell.fd, command.encode())
    records = shell.wait_for("hooks:")
    assert "hooks:_check_git_repo_change\n" in records


@needs_defer
def test_resourcing_recovers_interrupted_startup(shell: Shell) -> None:
    shell.start()
    _ = shell.wait_for("colorize-waiting")
    # Interrupt a task after the scheduler has removed its fd handler.
    # Send SIGINT directly, independent of ZLE's terminal signal mode.
    os.kill(shell.pid, signal.SIGINT)
    _ = shell.wait_for("colorize-unwound")
    # Finish discarding the interrupted line before the next command.
    _ = os.write(shell.fd, b"\n_test_snapshot interrupted\n")
    records = shell.wait_for("interrupted ")
    interrupted = next(
        line for line in records.splitlines() if line.startswith("interrupted ")
    )
    assert "pending=1" in interrupted
    assert "load:docker\n" not in records
    (shell.home / "continue").touch()
    _ = os.write(shell.fd, b'source "$ZDOTDIR/.zshrc"\n')
    lines = shell.wait_for("done ", 2).splitlines()
    assert_complete(next(line for line in lines if line.startswith("done ")))
    # Retry the interrupted task without replaying completed plugins or
    # duplicating the remaining queue.
    assert lines.count("load:aws") == 1
    assert lines.count("load:colorize") == 2
    assert lines.count("load:docker") == 1
    assert lines.count("load:git") == 1


def test_omz_revision_invalidates_completion_registrations(shell: Shell) -> None:
    omz = shell.home / ".oh-my-zsh"
    shell.write(".oh-my-zsh/completions/_old_test", "#compdef update-test\n")
    shell.git("init", "-q")
    shell.git("add", ".")
    shell.git("commit", "-qm", "Initial completion")
    query = 'print -r -- "${_comps[update-test]-missing}"'
    assert shell.run_command(query) == "_old_test"
    dump = next(shell.home.glob(".zcompdump-*"))
    initial_mtime = dump.stat().st_mtime_ns
    assert shell.run_command(query) == "_old_test"
    assert dump.stat().st_mtime_ns == initial_mtime

    # Both the fpath and completion file count remain unchanged.
    _ = (omz / "completions/_old_test").rename(omz / "completions/_new_test")
    shell.git("add", "-A")
    shell.git("commit", "-qm", "Rename completion function")
    assert shell.run_command(query) == "_new_test"

    shell.write(".oh-my-zsh/completions/_new_test", "#compdef renamed-test\n")
    shell.git("add", "-A")
    shell.git("commit", "-qm", "Change completion registration")
    assert shell.run_command(query) == "missing"
    query = 'print -r -- "${_comps[renamed-test]-missing}"'
    assert shell.run_command(query) == "_new_test"


def test_completion_security_handler_follows_compinit_audit(shell: Shell) -> None:
    shell.write(
        ".oh-my-zsh/lib/compfix.zsh",
        'handle_completion_insecurities() { print compfix >> "$ZDOTDIR/events"; }',
    )
    shell.write(".oh-my-zsh/custom/functions/_audit_test", "#compdef audit-test\n")
    completion_dir = shell.home / ".oh-my-zsh/custom/functions"
    completion_dir.chmod(0o777)  # Intentionally insecure fixture.
    query = 'print -r -- "${_comps[audit-test]-missing}"'
    assert shell.run_command(query) == "missing"
    assert shell.events.read_text().splitlines().count("compfix") == 1

    # Fix permissions and re-source in the same shell, so the flag from
    # the first audit must be reset before running the second one.
    shell.events.unlink()
    command = f"chmod 755 {shlex.quote(str(completion_dir))}; "
    command += 'source "$ZDOTDIR/.zshrc"; ' + query
    assert shell.run_command(command) == "_audit_test"
    assert shell.events.read_text().splitlines().count("compfix") == 1


def test_command_string_initializes_synchronously(shell: Shell) -> None:
    _ = shell.run_command("_test_snapshot command")
    records = shell.events.read_text().splitlines()
    assert_complete(next(line for line in records if line.startswith("command ")))
    assert "BAD-FZF-ORDER" not in records


def test_missing_scheduler_falls_back_to_synchronous_startup(shell: Shell) -> None:
    shell.env["ZSH_DEFER_DIR"] = str(shell.home / "missing-defer")
    shell.start()
    records = shell.wait_for("first ").splitlines()
    assert_complete(next(line for line in records if line.startswith("first ")))
