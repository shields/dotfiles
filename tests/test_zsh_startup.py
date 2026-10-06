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
import sys
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
        for name in (
            "ZSH_CUSTOM",
            "ZSH_CACHE_DIR",
            "ZSH_COMPDUMP",
            "STARSHIP_CONFIG",
            "__CF_USER_TEXT_ENCODING",
            "CLAUDE_CODE_OAUTH_TOKEN",
            "EDITOR",
            "GPG_TTY",
            "HOMEBREW_PREFIX",
            "TMUX",
            "VIRTUAL_ENV",
        ):
            _ = self.env.pop(name, None)
        self.set_ostype(None)
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

    def set_ostype(self, ostype: str | None) -> None:
        # zsh assigns OSTYPE before it reads any startup file, so only a
        # startup file can make it report another system.
        lines = ['HISTFILE="$ZDOTDIR/history"']
        if ostype:
            lines.append(f"OSTYPE={ostype}")
        self.write(".zshenv", "\n".join(lines) + "\n")

    def use_homebrew(self) -> Path:
        prefix = self.home / "brew"
        self.env["HOMEBREW_PREFIX"] = str(prefix)
        tomorrow = time.time() + 86400
        for name in ("brew-shellenv", "brew-shellenv-intel", "brew-shellenv-linux"):
            self.write(
                f"cache/zsh/{name}",
                f"export HOMEBREW_PREFIX={shlex.quote(str(prefix))}\n",
            )
            os.utime(self.home / "cache/zsh" / name, (tomorrow, tomorrow))
        return prefix

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

    def run(self, command: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [ZSH, "-ic", command],
            env=self.env,
            cwd=self.home,
            capture_output=True,
            text=True,
            timeout=15,
            check=False,
        )

    def run_command(self, command: str) -> str:
        result = self.run(command)
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


LINUX = "linux-gnu"
DARWIN = "darwin25.4.0"

EMACS_ALIASES = """
alias emacs="plugin-launcher --no-wait"
alias e=emacs
alias te="plugin-launcher -nw"
"""

CACHE_SCENARIO = """
_t_watch="$ZDOTDIR/watch"
_t_cache="$XDG_CACHE_HOME/zsh/ctime-test"
_t_builds=0
_t_build() { (( ++_t_builds )); print built; }
_t_step() {
    _startup_cached ctime-test "$_t_watch" -- _t_build >/dev/null
    print -r -- "$1 builds=$_t_builds" >> "$ZDOTDIR/events"
}
: > "$_t_watch"
command touch -t 200001010000 "$_t_watch"
_t_step missing
command touch -t 209901010000 "$_t_cache"
_t_step newer
command touch -t 202001010000 "$_t_cache"
_t_step older
"""


AFTER_ZSHRC = """
print -r -- "status=$? helper=${+functions[_startup_source]}" >> "$ZDOTDIR/events"
"""

CLIPBOARD_STATE = 'print -rl -- "$aliases[e]" "$+functions[pc]" "$+aliases[pc]"'


def executable(path: Path, content: str) -> None:
    _ = path.write_text(content)
    path.chmod(0o755)


def test_missing_omz_libraries_end_startup_with_a_message(shell: Shell) -> None:
    (shell.home / ".oh-my-zsh/lib/base.zsh").unlink()
    zshrc = shlex.quote(str(REPO / ".zshrc"))
    shell.write(".zshrc", f"source {zshrc}\n{AFTER_ZSHRC}")
    result = shell.run("print -r -- shell-survived")
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "shell-survived"
    assert f"zshrc: no oh-my-zsh libraries in {shell.home}/.oh-my-zsh/lib" in (
        result.stderr
    )
    assert shell.events.read_text() == "status=1 helper=0\n"


def test_cached_output_is_rebuilt_when_a_watch_changes_status(shell: Shell) -> None:
    shell.write(".oh-my-zsh/plugins/emacs/emacs.plugin.zsh", CACHE_SCENARIO)
    _ = shell.run_command("true")
    records = shell.events.read_text().splitlines()
    steps = [line for line in records if " builds=" in line]
    # The watch's modification time predates every cache state; only its
    # status-change time, which is the moment of the touch, is recent.
    assert steps == ["missing builds=1", "newer builds=1", "older builds=2"]


@pytest.mark.parametrize(
    ("ostype", "probed"),
    [(None, sys.platform == "linux"), (DARWIN, False), (LINUX, True)],
)
def test_linuxbrew_is_probed_only_on_linux(
    shell: Shell, ostype: str | None, *, probed: bool
) -> None:
    shell.set_ostype(ostype)
    # The trace prints PATH, which names Linuxbrew on a Linux host that has it.
    path = ":".join(
        entry
        for entry in shell.env["PATH"].split(":")
        if not entry.startswith("/home/linuxbrew")
    )
    result = subprocess.run(
        [ZSH, "-ixc", ":"],
        env={**shell.env, "PATH": path},
        cwd=shell.home,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert ("/home/linuxbrew" in result.stderr) is probed


def test_local_bin_is_on_path_only_on_linux(shell: Shell) -> None:
    home = str(shell.home)
    shell.set_ostype(LINUX)
    entries = shell.run_command("print -rl -- $path").splitlines()
    assert entries[:2] == [f"{home}/bin", f"{home}/.local/bin"]
    shell.set_ostype(DARWIN)
    entries = shell.run_command("print -rl -- $path").splitlines()
    assert entries[0] == f"{home}/bin"
    assert f"{home}/.local/bin" not in entries


@pytest.mark.parametrize(
    ("ostype", "editor", "encoding"),
    [
        (LINUX, "emacsclient --tty --alternate-editor=", "unset"),
        (
            DARWIN,
            "{home}/.oh-my-zsh/plugins/emacs/emacsclient.sh --create-frame",
            f"{os.getuid()}:134217984:134217984",
        ),
    ],
)
def test_editor_and_text_encoding_follow_the_system(
    shell: Shell, ostype: str, editor: str, encoding: str
) -> None:
    shell.set_ostype(ostype)
    values = shell.run_command(
        'print -rl -- "$EDITOR" "${__CF_USER_TEXT_ENCODING-unset}"'
    ).splitlines()
    assert values == [editor.format(home=shell.home), encoding]


def test_visual_follows_editor_on_linux(shell: Shell) -> None:
    shell.set_ostype(LINUX)
    shell.env["VISUAL"] = "vi"  # What .profile exports in every login shell.
    values = shell.run_command('print -rl -- "$EDITOR" "$VISUAL"').splitlines()
    assert values == ["emacsclient --tty --alternate-editor="] * 2


@pytest.mark.parametrize(
    ("ostype", "token", "expected"),
    [
        (LINUX, "oauth-token-value", "oauth-token-value"),
        (LINUX, None, "unset"),
        (DARWIN, "oauth-token-value", "unset"),
    ],
)
def test_claude_token_is_exported_from_its_file_only_on_linux(
    shell: Shell, ostype: str, token: str | None, expected: str
) -> None:
    shell.set_ostype(ostype)
    if token is not None:
        shell.write(".config/secrets/CLAUDE_CODE_OAUTH_TOKEN", token + "\n")
    value = shell.run_command('print -r -- "${CLAUDE_CODE_OAUTH_TOKEN-unset}"')
    assert value == expected


def test_rustup_and_python_come_from_the_homebrew_prefix(shell: Shell) -> None:
    prefix = shell.use_homebrew()
    shell.env["PATH"] = "/usr/bin:/bin"
    (prefix / "opt/rustup/bin").mkdir(parents=True)
    (prefix / "opt/python/libexec/bin").mkdir(parents=True)
    executable(
        prefix / "opt/python/libexec/bin/python", '#!/bin/sh\necho "brew-python $*"\n'
    )
    entries = shell.run_command("print -rl -- $path").splitlines()
    assert f"{prefix}/opt/rustup/bin" in entries
    assert "/opt/homebrew/opt/rustup/bin" not in entries
    assert shell.run_command("p -V") == "brew-python -V"
    (shell.home / "venv/bin").mkdir(parents=True)
    executable(shell.home / "venv/bin/python", '#!/bin/sh\necho "venv-python $*"\n')
    shell.env["VIRTUAL_ENV"] = str(shell.home / "venv")
    assert shell.run_command("p -V") == "venv-python -V"


def test_rustup_is_skipped_without_a_homebrew_prefix(shell: Shell) -> None:
    prefix = shell.use_homebrew()
    shell.env["PATH"] = "/usr/bin:/bin"
    entries = shell.run_command("print -rl -- $path").splitlines()
    assert f"{prefix}/opt/rustup/bin" not in entries
    assert "/opt/homebrew/opt/rustup/bin" not in entries


@pytest.mark.parametrize(
    ("ostype", "plugin", "e", "emacs"),
    [
        (LINUX, EMACS_ALIASES, "te", "te"),
        (LINUX, ":", "", ""),
        (DARWIN, EMACS_ALIASES, "emacs", "plugin-launcher --no-wait"),
    ],
)
def test_emacs_aliases_open_terminal_frames_on_linux(
    shell: Shell, ostype: str, plugin: str, e: str, emacs: str
) -> None:
    shell.set_ostype(ostype)
    shell.write(".oh-my-zsh/plugins/emacs/emacs.plugin.zsh", plugin)
    values = shell.run_command('print -r -- "$aliases[e]|$aliases[emacs]"')
    assert values.split("|") == [e, emacs]


def test_pc_copies_through_tmux_or_osc52_on_linux(shell: Shell) -> None:
    shell.set_ostype(LINUX)
    (shell.home / "bin").mkdir()
    record_tmux = (
        '#!/bin/sh\nprintf "%s\\n" "$*" > "$HOME/tmux-args"\ncat > "$HOME/tmux-stdin"\n'
    )
    executable(shell.home / "bin/tmux", record_tmux)
    result = shell.run("printf hello | pc")
    assert result.returncode == 0, result.stderr
    assert "\x1b]52;c;aGVsbG8=\x07" in result.stdout
    assert not (shell.home / "tmux-args").exists()
    shell.env["TMUX"] = f"{shell.home}/tmux-socket,1,0"
    assert shell.run_command("printf hello | pc") == ""
    assert (shell.home / "tmux-args").read_text() == "load-buffer -w -\n"
    assert (shell.home / "tmux-stdin").read_text() == "hello"
    assert shell.run_command('print -r -- "$+aliases[pv]$+functions[pv]"') == "00"


def test_pc_and_pv_are_the_pasteboard_commands_on_the_mac(shell: Shell) -> None:
    shell.set_ostype(DARWIN)
    expected = (
        ("pbcopy", "pbpaste") if os.access("/usr/bin/pbcopy", os.X_OK) else ("", "")
    )
    values = shell.run_command('print -r -- "$aliases[pc]|$aliases[pv]"')
    assert tuple(values.split("|")) == expected


@pytest.mark.parametrize("ostype", [LINUX, DARWIN])
def test_sourcing_again_keeps_the_clipboard_commands(shell: Shell, ostype: str) -> None:
    shell.set_ostype(ostype)
    shell.write(".oh-my-zsh/plugins/emacs/emacs.plugin.zsh", EMACS_ALIASES)
    first = shell.run_command(CLIPBOARD_STATE)
    again = shell.run_command(f'source "$ZDOTDIR/.zshrc"; {CLIPBOARD_STATE}')
    assert again == first


def test_gpg_tty_is_the_terminal(shell: Shell) -> None:
    shell.start()
    _ = shell.wait_for("first ")
    command = 'print -r -- "gpg=$GPG_TTY tty=$TTY" >> "$ZDOTDIR/events"\n'
    _ = os.write(shell.fd, command.encode())
    records = shell.wait_for("gpg=")
    line = next(line for line in records.splitlines() if line.startswith("gpg="))
    gpg, tty = (field.partition("=")[2] for field in line.split(" "))
    assert tty.startswith("/dev/")
    assert gpg == tty
