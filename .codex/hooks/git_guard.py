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

# ruff: noqa: INP001 - standalone hook, not an importable package

import json
import re
import shlex
import sys
from pathlib import PurePosixPath
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterator

NO_PUSH = "The user performs all pushes. Leave commits local."
KEEP_HOOKS = (
    "Respect the configured Git hooks. "
    "Commit without bypass flags or configuration overrides."
)

# Keep operators distinct from quoted arguments such as a commit message ';'.
TOKEN = re.compile(
    r"(?P<space>[ \t\r]+|\\\n)"
    r"|(?P<comment>\#[^\n]*)"
    r"|(?P<operator><<<|<<-|<<|[;&|()<>\n])"
    r"|(?P<word>(?:[^\s;&|()<>\"'\\]+|\\[\s\S]|'[^']*'|\"(?:[^\"\\]|\\[\s\S])*\")+)"
)
ASSIGNMENT = re.compile(r"[A-Za-z_][A-Za-z_0-9]*=")
GIT_VALUE_OPTIONS = {
    "-C",
    "-c",
    "--git-dir",
    "--work-tree",
    "--namespace",
    "--config-env",
    "--super-prefix",
    "--attr-source",
}
COMMIT_VALUE_OPTIONS = {
    "--message",
    "--file",
    "--reuse-message",
    "--reedit-message",
    "--author",
    "--date",
    "--template",
    "--cleanup",
    "--fixup",
    "--squash",
    "--trailer",
    "--pathspec-from-file",
}


def commands(script: str) -> Iterator[list[str]]:
    words: list[str] = []
    heredocs: list[tuple[str, bool]] = []
    pending_heredoc: str | None = None
    offset = 0
    while offset < len(script):
        token = TOKEN.match(script, offset)
        if token is None:
            msg = "Could not check Git policy. Use a simple shell command."
            raise ValueError(msg)
        offset = token.end()
        raw = token.group()
        if token.lastgroup in {"space", "comment"}:
            continue
        if pending_heredoc and token.lastgroup != "word":
            msg = "Missing here-document delimiter; cannot check Git policy."
            raise ValueError(msg)
        if token.lastgroup == "word":
            word = shlex.split(raw)[0]
            if pending_heredoc:
                heredocs.append((word, pending_heredoc == "<<-"))
                pending_heredoc = None
            else:
                words.append(word)
            continue
        if raw in {"<<", "<<-"}:
            pending_heredoc = raw
        elif raw in {"<", ">", "<<<"}:
            words.append(raw)
        else:
            if words:
                yield words
                words = []
            if raw == "\n":
                # Here-document bodies are data, not new shell commands.
                for delimiter, strip_tabs in heredocs:
                    prefix = r"\t*" if strip_tabs else ""
                    end = re.search(
                        rf"(?m)^{prefix}{re.escape(delimiter)}(?:\n|$)",
                        script[offset:],
                    )
                    if end is None:
                        msg = "Unterminated here-document; cannot check Git policy."
                        raise ValueError(msg)
                    offset += end.end()
                heredocs = []
    if pending_heredoc:
        msg = "Missing here-document delimiter; cannot check Git policy."
        raise ValueError(msg)
    if words:
        yield words


def bypasses_hooks(args: list[str]) -> bool:
    index = 0
    while index < len(args):
        arg = args[index]
        index += 1
        if arg == "--":
            break
        if arg == "--no-verify":
            return True
        if arg in COMMIT_VALUE_OPTIONS:
            index += 1
        elif arg.startswith("-") and not arg.startswith("--"):
            for position, flag in enumerate(arg[1:], start=1):
                if flag == "n":
                    return True
                if flag in "mFCct":
                    if position == len(arg) - 1:
                        index += 1
                    break
    return False


def git_reason(args: list[str]) -> str | None:
    index = 0
    has_config = False
    while index < len(args):
        arg = args[index]
        # Git rejects global '--'; bare '--exec-path' prints a path and exits.
        # Only '--exec-path=<dir>' continues to a subcommand.
        if arg in {"--", "--exec-path"}:
            return None
        if not arg.startswith("-"):
            break
        if arg in {"-c", "--config-env"} or arg.startswith(("-c", "--config-env=")):
            has_config = True
        index += 2 if arg in GIT_VALUE_OPTIONS else 1
    if index >= len(args):
        return None
    subcommand = args[index]
    if subcommand == "push":
        return NO_PUSH
    if subcommand == "commit" and (has_config or bypasses_hooks(args[index + 1 :])):
        return KEEP_HOOKS
    return None


def command_reason(words: list[str]) -> str | None:
    index = 0
    while index < len(words):
        word = words[index]
        executable = PurePosixPath(word).name
        if ASSIGNMENT.match(word) or word in {
            "if",
            "then",
            "elif",
            "else",
            "while",
            "until",
            "do",
            "!",
            "{",
        }:
            index += 1
        elif executable in {"command", "exec", "env", "time"}:
            index += 1
            while index < len(words) and words[index].startswith("-"):
                option = words[index]
                if executable == "command" and option in {"-v", "-V"}:
                    return None
                index += (
                    2
                    if executable == "env"
                    and option in {"-u", "--unset", "-C", "--chdir"}
                    else 1
                )
        else:
            break
    if index >= len(words):
        return None
    executable = PurePosixPath(words[index]).name
    args = words[index + 1 :]
    if executable == "git":
        return git_reason(args)
    if executable in {"bash", "zsh", "sh"}:
        for position, arg in enumerate(args):
            if not arg.startswith("-"):
                break
            if not arg.startswith("--") and "c" in arg and position + 1 < len(args):
                return shell_reason(args[position + 1])
    return None


def shell_reason(script: str) -> str | None:
    for words in commands(script):
        reason = command_reason(words)
        if reason:
            return reason
    return None


def main() -> None:
    try:
        payload = json.load(sys.stdin)
        if payload.get("tool_name") != "Bash":
            return
        reason = shell_reason(payload["tool_input"]["command"])
    except Exception as exc:  # noqa: BLE001 - hook errors otherwise fail open
        reason = (
            f"Git policy check failed ({type(exc).__name__}). "
            "Use a simple shell command."
        )
    if reason:
        json.dump(
            {
                "hookSpecificOutput": {
                    "hookEventName": "PreToolUse",
                    "permissionDecision": "deny",
                    "permissionDecisionReason": reason,
                }
            },
            sys.stdout,
        )
        print()


if __name__ == "__main__":
    main()
