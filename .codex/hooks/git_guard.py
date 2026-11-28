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
import signal
import sys
from enum import IntEnum
from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, Self

if TYPE_CHECKING:
    from collections.abc import Iterator
    from types import FrameType

# Monitor runs its command under the Bash permission rules.
SHELL_TOOLS = {"Bash", "Monitor"}

# Seconds the hook may run before it denies. hooks.json gives it 10, and Codex
# runs the command when a hook outlasts that.
WALL_BUDGET = 5.0

NO_PUSH = "The user performs all pushes. Leave commits local."
KEEP_HOOKS = (
    "Respect the configured Git hooks. "
    "Commit without bypass flags or configuration overrides."
)
NO_GITHUB_WRITES = (
    "The user manages GitHub state. Use gh api only for GET or HEAD requests; "
    "-f, -F and --input send a POST unless the request names --method GET."
)
NO_GRAPHQL_WRITES = (
    "The user manages GitHub state. Give gh api graphql its query as a quoted "
    "literal that is not a mutation."
)
UNREADABLE = (
    "Could not read this command well enough to check it. Use literal words, and "
    'expand only "$name" or "${name}" inside double quotes, as in -C "$repo" or '
    '"repos/$owner/$repo". Do not use command substitution or backticks.'
)
NO_CLOUDFLARE_RISK = (
    "Cloudflare auth, credential, billing, registrar, membership and delete "
    "commands are run by the user. Ask them to run it."
)
CF_UNREADABLE = (
    "Could not read this cf command well enough to check it. Use literal words "
    "(spell out each ID, and write xargs -I % rather than -I{}), and expand "
    '"$name" only inside double quotes as an option\'s value written with =, as '
    'in --name="$name", or as the value of -z, --zone or --profile.'
)

WORD = r"(?P<word>(?:[^\s;&|()<>\"'\\]+|\\[\s\S]|'[^']*'|\"(?:[^\"\\]|\\[\s\S])*\")+)"
# Keep operators distinct from quoted arguments such as a commit message ';'. The
# & of 2>&1 and the | of >| belong to their redirections, and end no command.
TOKEN = re.compile(
    r"(?P<space>[ \t\r]+|\\\n)"
    r"|(?P<comment>\#[^\n]*)"
    r"|(?P<operator><<<|<<-|<<|&>>|&>|[<>]&|>\||[;&|()<>\n])"
    rf"|{WORD}"
)
GLUED = re.compile(WORD)
ASSIGNMENT = re.compile(r"[A-Za-z_][A-Za-z_0-9]*(?:\[[^\]]*\])?\+?=")
DESCRIPTOR = re.compile(r"[0-9]+")
REDIRECTIONS = {"<", ">", "<<<", ">&", "<&", "&>", "&>>", ">|"}
PIECE = re.compile(
    r"(?P<single>'[^']*')"
    r"|(?P<double>\"(?:[^\"\\]|\\[\s\S])*\")"
    r"|(?P<escape>\\[\s\S])"
    r"|(?P<bare>[^\"'\\]+)"
)
PLAIN = re.compile(r"[\w@%+=:,./-]*")
DOUBLE_ESCAPE = re.compile(r'\\([\\"])|\\\n')
# The expansions that stay one word inside double quotes in bash and zsh; a
# subscript such as $name[@] or a flag such as ${=name} can make several. An
# escaped $ or backtick is one character.
ONE_WORD = re.compile(
    r"(?:\\[^\n]|\$(?:[A-Za-z_]\w*+(?!\[)|\{[A-Za-z_]\w*\})|[^$`\\])*+"
)
# Inside double quotes the word text keeps a backslash that the shell drops
# before these, and a backslash-newline joins lines. So a nested script may run
# an expansion, or a word, that reads as escaped here.
UNSETTLED_ESCAPES = "$`\n"
# Overrides that cannot keep a hook from running.
IDENTITY = re.compile(
    r"(?:user\.(?:name|email)|(?:commit|tag)\.gpgsign)=.*", re.IGNORECASE | re.DOTALL
)
POSITIONAL = re.compile(r"[\w/.]")
MUTATION = re.compile(r"\bmutation\b")
QUERY_START = re.compile(r"\s*(?:query\b|\{)")
GIT_VALUE_OPTIONS = {
    "-C",
    "-c",
    "--git-dir",
    "--work-tree",
    "--namespace",
    "--config-env",
    "--super-prefix",
    "--attr-source",
    "--shallow-file",
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
# Every option of gh api that takes a value (gh api has no -R). After a short
# one the rest of its cluster is that value.
GH_API_SHORT = {
    "X": "method",
    "F": "field",
    "f": "raw-field",
    "H": "other",
    "q": "other",
    "t": "other",
    "p": "other",
}
GH_API_LONG = {
    "--method": "method",
    "--field": "field",
    "--raw-field": "raw-field",
    "--input": "input",
    "--header": "other",
    "--jq": "other",
    "--template": "other",
    "--preview": "other",
    "--hostname": "other",
    "--cache": "other",
}
READ_METHODS = {"GET", "HEAD"}
SHELL_VALUE_OPTIONS = {"--emulate", "--init-file", "--rcfile"}
KEYWORDS = {"if", "then", "elif", "else", "while", "until", "do", "!", "{", "coproc"}
BEFORE_CASE = KEYWORDS | {"time"}
# Wrappers whose first word after the options is a count or a duration.
COUNTED = {"timeout", "repeat"}
# Commands that run the command after their options, each with the options that
# take a separate value. Claude Code's own permission rules look through them.
WRAPPERS: dict[str, set[str]] = {
    "command": set(),
    "exec": {"-a"},
    "env": {"-u", "--unset", "-C", "--chdir", "-P"},
    "time": {"-f", "-o", "--format", "--output"},
    "nohup": set(),
    "nice": {"-n", "--adjustment"},
    "timeout": {"-k", "--kill-after", "-s", "--signal"},
    "stdbuf": {"-i", "-o", "-e", "--input", "--output", "--error"},
    "builtin": set(),
    "noglob": set(),
    "nocorrect": set(),
    "setsid": set(),
    "repeat": set(),
    "xargs": {
        "-a",
        "-d",
        "-E",
        "-I",
        "-J",
        "-L",
        "-n",
        "-P",
        "-R",
        "-S",
        "-s",
        "--arg-file",
        "--delimiter",
        "--max-args",
        "--max-chars",
        "--max-lines",
        "--max-procs",
        "--process-slot-var",
    },
    "sudo": {
        "-a",
        "-C",
        "-c",
        "-D",
        "-g",
        "-h",
        "-p",
        "-R",
        "-r",
        "-T",
        "-t",
        "-U",
        "-u",
        "--chdir",
        "--chroot",
        "--close-from",
        "--group",
        "--host",
        "--other-user",
        "--prompt",
        "--role",
        "--type",
        "--user",
    },
    "doas": {"-u", "-C"},
}
# Global options of cf (yargs). An option in neither set may take the next
# word as its value.
CF_VALUE_FLAGS = {
    "-z",
    "--zone",
    "-m",
    "--mode",
    "--profile",
    "--event-code",
    "--persist-to",
    "--persistTo",
    "--eventCode",
}
CF_BOOLEAN_FLAGS = {
    "-q",
    "--quiet",
    "-v",
    "--version",
    "--local",
    "--temporary-account",
    "--temporaryAccount",
    "--dry-run",
    "--dryRun",
}
# Every spelling of cf's help flag (yargs help, alias h) except a bare --help or
# -h: --no-help, --h, --no-h, --help=x, -h=x, and a short group that contains h,
# such as -qh=false or -qh false. The last occurrence wins and most of these
# switch help off, so any of them cancels an earlier --help.
CF_HELP_SPELLING = re.compile(r"--(?:no-)?h(?:elp)?(?:[=.]|$)|-[^-]*h")
# Commands that search the docs or print help, schemas and completions, or change
# only cf's own telemetry setting; none operates on a resource.
CF_SAFE_COMMANDS = {"docs", "cli", "schema", "complete"}
CF_AUTH_VERBS = {"create", "delete", "activate", "deactivate"}
# Verb words that delete, purge, flush, overwrite or dismantle; 'disable' covers
# two DELETE endpoints (email-routing, r2 sippy) and fails closed for the rest.
CF_DELETE_VERB = re.compile(r"(?:^|-)(?:delete|purge)(?:-|$)")
CF_DESTRUCTIVE_VERBS = {
    "clear",
    "cleanup",
    "disable",
    "disconnect",
    "empty",
    "invalidate",
    "invalidate-environment",
    "opt-out-operator-price",
    "remove",
    "restore",
    "revoke",
    "terminate",
    "ungroup",
}
# Resources that hold credentials, and verbs that mint, rotate or store them.
# `cf login` is a hidden alias of `cf auth login`, and cloudflared's logins
# download a certificate or store a token.
CF_CREDENTIAL_WORD = re.compile(
    r"(?:^|-)(?:tokens?|secrets?|credentials?|credential-sets|keys|certificates?)$"
)
CF_CREDENTIAL_VERBS = {
    "login",
    "logout",
    "roll",
    "rotate",
    "rotate-seed",
    "psk-generate",
    "psk-set",
    "ssh-gen",
}
# Resources whose read commands return the secret itself, and options that
# upload secrets along with another command.
CF_SECRET_RESOURCES = {"token"}
CF_SECRET_OPTIONS = {"--secrets-file", "--secretsFile"}
# Where 'keys' and 'credentials' name data or reports, not credentials.
CF_DATA_RESOURCES = {"kv", "radar"}
CF_MONEY_AND_MEMBERS = {
    ("billing",),
    ("monetization",),
    ("pay-per-crawl",),
    ("pay-per-use",),
    ("registrar",),
    ("registrar-sandbox",),
    ("scim",),
    ("accounts", "billing"),
    ("accounts", "members"),
    ("accounts", "subscriptions"),
    ("accounts", "ut-billing"),
    ("iam", "oauth-clients"),
    ("iam", "sso"),
    ("iam", "user-groups"),
    ("organizations", "billing"),
    ("organizations", "invites"),
    ("organizations", "members"),
    ("user", "billing"),
    ("user", "invites"),
    ("user", "memberships"),
    ("user", "subscriptions"),
    ("zones", "subscriptions"),
}
CF_READ_VERBS = {
    "get",
    "list",
    "search",
    "check",
    "validate",
    "query",
    "verify",
    "status",
    "transfer-check",
}
# Verbs, and the compound verbs that start with them, of commands that change a
# resource without deleting it.
CF_WRITE_VERB = re.compile(
    r"(?:create|update|edit|put|patch|set|replace|add|upload|refresh|generate"
    r"|duplicate|bulk|pay|toggle|cancel|transfer-in|enable|activate|deactivate"
    r"|rotate|roll|revoke|respond|begin)(?:-|$)"
)


class Level(IntEnum):
    """How much of a word's value its text settles."""

    LITERAL = 0
    ONE_WORD = 1
    UNKNOWN = 2


class Word(str):
    """A shell word with its quoting removed.

    A word that touches is written directly after a substitution, so it may be
    the rest of that word.
    """

    __slots__ = ("level", "touches")

    level: Level
    touches: bool

    def __new__(
        cls, text: str, level: Level = Level.LITERAL, *, touches: bool = False
    ) -> Self:
        word = super().__new__(cls, text)
        word.level = level
        word.touches = touches
        return word


class Redirection(Word):
    """A redirection operator, which takes the next word as its target."""

    __slots__ = ()


class Substitution(Word):
    """A command or process substitution, which may expand to no word."""

    __slots__ = ()


def with_joins(words: list[Word]) -> Iterator[list[Word]]:
    """The words, then, if any touches a substitution, the words with that joined to it.

    $(pwd)/sub is one word when the substitution expands to something, and the
    words $(pwd) and /sub when it expands to nothing.
    """
    yield words
    if any(word.touches for word in words):
        joined: list[Word] = []
        for word in words:
            if word.touches and joined:
                joined[-1] = Substitution(
                    joined[-1] + word, Level.UNKNOWN, touches=joined[-1].touches
                )
            else:
                joined.append(word)
        yield joined


def without_redirections(words: list[Word]) -> list[Word]:
    kept: list[Word] = []
    target = False
    for word in words:
        if isinstance(word, Redirection):
            target = True
        elif target:
            target = False
        else:
            kept.append(word)
    return kept


def level_of(raw: str) -> Level:
    level = Level.LITERAL
    for piece in PIECE.finditer(raw):
        text = piece.group()
        if piece.lastgroup == "bare" and not PLAIN.fullmatch(text):
            return Level.UNKNOWN
        if piece.lastgroup == "escape" and text[1] in UNSETTLED_ESCAPES:
            return Level.UNKNOWN
        if piece.lastgroup == "double":
            if not ONE_WORD.fullmatch(text[1:-1]):
                return Level.UNKNOWN
            if "$" in text or "`" in text:
                level = Level.ONE_WORD
    return level


def unquoted(raw: str) -> str:
    """The word that raw spells.

    A backslash-newline vanishes, as in the shell, and a backslash inside double
    quotes stays unless it precedes a quote or a backslash.
    """
    parts: list[str] = []
    for piece in PIECE.finditer(raw):
        text = piece.group()
        if piece.lastgroup == "single":
            parts.append(text[1:-1])
        elif piece.lastgroup == "double":
            parts.append(DOUBLE_ESCAPE.sub(lambda match: match[1] or "", text[1:-1]))
        elif piece.lastgroup == "escape":
            parts.append(text[1:] if text != "\\\n" else "")
        else:
            parts.append(text)
    return "".join(parts)


def commands(script: str) -> Iterator[list[Word]]:
    words: list[Word] = []
    # Each open ( is a substitution, which suspends the command around it until
    # its ), or a group. A ) that ends a case pattern closes nothing, so the
    # case statements that each level has open are counted.
    substitutions: list[bool] = []
    suspended: list[list[Word]] = []
    cases = [0]
    closed = -1
    heredocs: list[tuple[str, bool]] = []
    pending_heredoc: str | None = None
    offset = 0
    while offset < len(script):
        token = TOKEN.match(script, offset)
        if token is None:
            msg = "Could not check command policy. Use a simple shell command."
            raise ValueError(msg)
        if token.lastgroup == "comment" and token.start() == closed:
            # A hash that touches a substitution continues its word.
            token = GLUED.match(script, token.start()) or token
        offset = token.end()
        raw = token.group()
        if token.lastgroup in {"space", "comment"}:
            continue
        if pending_heredoc and token.lastgroup != "word":
            msg = "Missing here-document delimiter; cannot check command policy."
            raise ValueError(msg)
        if token.lastgroup == "word":
            word = Word(unquoted(raw), level_of(raw), touches=token.start() == closed)
            if pending_heredoc:
                heredocs.append((word, pending_heredoc == "<<-"))
                pending_heredoc = None
            elif not (
                DESCRIPTOR.fullmatch(raw) and script.startswith(("<", ">"), offset)
            ):
                # A number that touches a redirection names its descriptor.
                if word == "case" and all(before in BEFORE_CASE for before in words):
                    cases[-1] += 1
                elif word == "esac" and not words:
                    cases[-1] = max(cases[-1] - 1, 0)
                words.append(word)
            continue
        if raw in {"<<", "<<-"}:
            pending_heredoc = raw
        elif raw in REDIRECTIONS:
            words.append(Redirection(raw))
        elif raw == "(" and script[token.start() - 1 : token.start()] in {
            "$",
            "<",
            ">",
        }:
            suspended.append(words)
            substitutions.append(True)
            cases.append(0)
            words = []
        else:
            if raw == "(":
                substitutions.append(False)
                cases.append(0)
            if words:
                yield from with_joins(without_redirections(words))
                words = []
            if raw == ")" and substitutions and cases[-1] == 0:
                cases.pop()
                if substitutions.pop():
                    words = suspended.pop()
                    # The $, < or > that opens the substitution belongs to its
                    # word.
                    if words and (
                        isinstance(words[-1], Redirection) or words[-1].endswith("$")
                    ):
                        words[-1] = Substitution(
                            f"{words[-1]}(...)",
                            Level.UNKNOWN,
                            touches=words[-1].touches,
                        )
                    else:
                        words.append(Substitution("(...)", Level.UNKNOWN))
                    closed = offset
            if raw == "\n":
                # Here-document bodies are data, not new shell commands.
                for delimiter, strip_tabs in heredocs:
                    prefix = r"\t*" if strip_tabs else ""
                    end = re.search(
                        rf"(?m)^{prefix}{re.escape(delimiter)}(?:\n|$)",
                        script[offset:],
                    )
                    if end is None:
                        msg = "Unterminated here-document; cannot check command policy."
                        raise ValueError(msg)
                    offset += end.end()
                heredocs = []
    if pending_heredoc:
        msg = "Missing here-document delimiter; cannot check command policy."
        raise ValueError(msg)
    for command in (*suspended, words):
        if command:
            yield from with_joins(without_redirections(command))


def commit_reason(args: list[Word], *, strict: bool) -> str | None:
    index = 0
    readable = True
    while index < len(args):
        arg = args[index]
        index += 1
        if arg == "--":
            break
        # git resolves any unambiguous prefix of a long option.
        if arg.startswith("--no-v") and "--no-verify".startswith(arg):
            return KEEP_HOOKS
        readable = readable and arg.level is Level.LITERAL
        takes_value = (
            arg.startswith("--")
            and "=" not in arg
            and any(name.startswith(arg) for name in COMMIT_VALUE_OPTIONS)
        )
        if arg.startswith("-") and not arg.startswith("--"):
            for position, flag in enumerate(arg[1:], start=1):
                if flag == "n":
                    return KEEP_HOOKS
                if flag in "mFCct":
                    takes_value = position == len(arg) - 1
                    break
                if flag in "Su":
                    # The rest of the cluster is a key ID or a mode.
                    break
        if takes_value and index < len(args):
            readable = readable and args[index].level is not Level.UNKNOWN
            index += 1
    return UNREADABLE if strict and not readable else None


def option_readable(option: Word, value: Word | None) -> bool:
    """Whether an option and its separate value read the way their text shows."""
    return option.level is Level.LITERAL and (
        value is None or value.level is not Level.UNKNOWN
    )


def defines_alias(setting: str) -> bool:
    """Whether a -c setting may define an alias, which can run any command."""
    key = setting.partition("=")[0]
    return key.lower().startswith("alias.") or "$" in key or "`" in key


def git_reason(args: list[Word]) -> str | None:
    index = 0
    has_config = False
    identity = False
    hidden = False
    readable = True
    settings: list[str] = []
    while index < len(args):
        arg = args[index]
        if isinstance(arg, Substitution):
            # It may expand to nothing, to an option or to the subcommand.
            hidden = True
            readable = False
            index += 1
            continue
        # Git rejects global '--'; bare '--exec-path' prints a path and exits.
        # Only '--exec-path=<dir>' continues to a subcommand.
        if arg in {"--", "--exec-path"}:
            return None
        if not arg.startswith("-"):
            break
        takes_value = arg in GIT_VALUE_OPTIONS
        value = args[index + 1] if takes_value and index + 1 < len(args) else None
        readable = readable and option_readable(arg, value)
        if arg.startswith("-c"):
            setting = value if arg == "-c" else arg[2:]
            if setting is not None and IDENTITY.fullmatch(setting):
                identity = True
            else:
                has_config = True
                settings.append(setting or "")
        elif arg == "--config-env" or arg.startswith("--config-env="):
            has_config = True
            settings.append((value or arg.partition("=")[2]) or "")
        index += 2 if takes_value else 1
    if index >= len(args):
        return None
    subcommand = args[index]
    if subcommand == "push" and not Path("/etc/cog-gateway").is_file():
        return NO_PUSH
    # An alias override can make any word run a commit, and a subcommand that the
    # text does not settle may be one.
    commits = (
        subcommand == "commit"
        or hidden
        or subcommand.level is not Level.LITERAL
        or any(defines_alias(setting) for setting in settings)
        or (not readable and any("commit" in arg for arg in args[index:]))
    )
    if has_config and commits:
        return KEEP_HOOKS
    if identity and commits and not (readable and subcommand.level is Level.LITERAL):
        return UNREADABLE
    if subcommand == "commit":
        return commit_reason(args[index + 1 :], strict=identity)
    return None


def graphql_reason(fields: list[tuple[str, Word | None]]) -> str | None:
    interpolated = False
    names: set[str] = set()
    for kind, value in fields:
        if value is None:
            return NO_GRAPHQL_WRITES
        name, _, text = value.partition("=")
        names.add(name)
        if value.level is not Level.LITERAL:
            if "$" in name or (name == "query" and not QUERY_START.match(text)):
                return NO_GRAPHQL_WRITES
            interpolated = interpolated or name == "query"
        # --field reads the value after @ as a file name.
        if name == "query" and (
            (kind == "field" and text.startswith("@")) or MUTATION.search(text)
        ):
            return NO_GRAPHQL_WRITES
    # A document that starts with a query can hold a mutation only as a second
    # operation, which GitHub runs only when operationName selects it.
    if interpolated and "operationName" in names:
        return NO_GRAPHQL_WRITES
    return None if "query" in names else NO_GRAPHQL_WRITES


def api_reason(args: list[Word]) -> str | None:
    methods: list[Word | None] = []
    fields: list[tuple[str, Word | None]] = []
    endpoint: Word | None = None
    has_input = False
    readable = True
    index = 0
    while index < len(args):
        arg = args[index]
        index += 1
        kind = None
        attached = None
        if arg.startswith("--"):
            name, equals, rest = arg.partition("=")
            kind = GH_API_LONG.get(name)
            if kind is None and name.startswith("--method"):
                methods.append(None)
            if equals:
                attached = rest
        elif arg.startswith("-") and arg != "-":
            for position, flag in enumerate(arg[1:], start=1):
                kind = GH_API_SHORT.get(flag)
                if kind is not None:
                    attached = arg[position + 1 :] or None
                    break
        else:
            endpoint = endpoint or arg
            readable = readable and (
                arg.level is Level.LITERAL
                or (arg.level is Level.ONE_WORD and POSITIONAL.match(arg) is not None)
            )
            continue
        if kind is None:
            readable = readable and arg.level is Level.LITERAL
            continue
        if attached is not None:
            value = Word(attached, arg.level)
            readable = readable and (
                arg.level is Level.LITERAL
                or (arg.level is Level.ONE_WORD and arg.startswith("--"))
            )
        else:
            value = args[index] if index < len(args) else None
            index += 1
            readable = readable and option_readable(arg, value)
        if kind == "method":
            methods.append(value)
        elif kind == "input":
            has_input = True
        elif kind != "other":
            fields.append((kind, value))
    graphql = endpoint == "graphql" and endpoint.level is Level.LITERAL
    if methods:
        for method in methods:
            if (
                method is None
                or method.level is not Level.LITERAL
                or method.upper() not in READ_METHODS
            ):
                return NO_GITHUB_WRITES
        if not graphql or not (fields or has_input):
            return None if readable else UNREADABLE
    elif not fields and not has_input:
        return None
    elif not graphql:
        return NO_GITHUB_WRITES
    if has_input:
        return NO_GRAPHQL_WRITES
    if not readable:
        return UNREADABLE
    return graphql_reason(fields)


def gh_reason(args: list[Word]) -> str | None:
    index = 0
    # gh's only root options are --help and --version; 'gh -R repo api' is an
    # error, so nothing before the subcommand takes a value.
    while index < len(args) and (
        args[index].startswith("-") or isinstance(args[index], Substitution)
    ):
        index += 1
    if index >= len(args) or args[index] != "api":
        return None
    return api_reason(args[index + 1 :])


def takes_value(wrapper: str, option: str) -> bool:
    """Whether the next word is the value of a wrapper's option or short cluster."""
    names = WRAPPERS[wrapper]
    if option in names:
        return True
    if option.startswith("--"):
        return False
    # In a cluster such as -tn the first flag that takes a value ends the scan:
    # the rest of the cluster is its value, as in -uroot, and only a flag in last
    # place takes the next word.
    for position, flag in enumerate(option[1:], start=1):
        if f"-{flag}" in names:
            return position == len(option) - 1
    return False


def program(word: str) -> str:
    """The command a word names; zsh expands =name to the path of name.

    macOS's file system folds case as Unicode does, so CF, ſh and cloudﬂare run
    cf, sh and cloudflare there.
    """
    return PurePosixPath(word.removeprefix("=")).name.casefold()


def nested_script(executable: str, args: list[Word]) -> Word | None:
    command = False
    index = 0
    while index < len(args) and args[index].startswith(("-", "+")):
        arg = args[index]
        index += 1
        if arg in {"-", "--"}:
            break
        if arg.startswith("--"):
            index += arg in SHELL_VALUE_OPTIONS
        elif executable == "zsh":
            # An option name that follows -o in the same word is its value.
            head, o, tail = arg.partition("o")
            command = command or "c" in head
            index += bool(o) and not tail
        else:
            command = command or "c" in arg
            index += arg.endswith(("o", "O"))
    return args[index] if command and index < len(args) else None


class OptionValue(str):
    """A word that an option the hook does not know may have taken as its value."""

    __slots__ = ()


def cf_readable(args: list[Word]) -> bool:
    """Whether each word that may be a command word reads as its text shows.

    A "$name" in double quotes stays one word, so it may be the value of a
    global option or of an option written with =; anywhere else it may be a
    command word that the hook cannot see. Words after -- are not commands.
    """
    index = 0
    while index < len(args):
        arg = args[index]
        index += 1
        if isinstance(arg, Substitution) or arg.level is Level.UNKNOWN:
            return False
        if arg == "--":
            return True
        name, equals, _ = arg.partition("=")
        inline = arg.startswith("-") and bool(equals) and "$" not in name
        if arg.level is Level.ONE_WORD and not inline:
            return False
        if arg in CF_VALUE_FLAGS and index < len(args):
            value = args[index]
            index += 1
            if isinstance(value, Substitution) or value.level is Level.UNKNOWN:
                return False
    return True


def cf_words(args: list[Word]) -> list[str]:
    words: list[str] = []
    help_requested = False
    takes_next = False
    index = 0
    while index < len(args):
        arg = args[index]
        index += 1
        if arg == "--":
            break
        # cf accepts a command's own options before, among and after its command
        # words, as in `cf --scopes x auth login` and `cf accounts --name x
        # tokens create`, so scanning continues past an option it does not know
        # and checks that option's value too. A boolean option takes a literal
        # true or false as its value, and so does --help; a --no- negation takes
        # no value.
        if not arg.startswith("-") or arg == "-":
            value = takes_next or arg in {"true", "false"}
            words.append(OptionValue(arg) if value else arg)
            takes_next = False
        elif arg in {"-h", "--help"}:
            help_requested = args[index : index + 1] != ["false"]
            takes_next = False
        elif arg in CF_VALUE_FLAGS:
            index += 1
            takes_next = False
        else:
            if CF_HELP_SPELLING.match(arg):
                help_requested = False
            takes_next = not (
                "=" in arg or arg in CF_BOOLEAN_FLAGS or arg.startswith("--no-")
            )
    return [] if help_requested else words


def cf_reason(args: list[Word]) -> str | None:
    if not cf_readable(args):
        return CF_UNREADABLE
    words = cf_words(args)
    if not words:
        return None
    first = next((word for word in words if not isinstance(word, OptionValue)), None)
    if first in CF_SAFE_COMMANDS:
        return None
    if any(arg.partition("=")[0] in CF_SECRET_OPTIONS for arg in args):
        return NO_CLOUDFLARE_RISK
    auth = cf_after(words, ("auth",))
    if auth is not None and not CF_AUTH_VERBS.isdisjoint(auth):
        return NO_CLOUDFLARE_RISK
    if any(
        CF_DELETE_VERB.search(word)
        or word in CF_DESTRUCTIVE_VERBS
        or word in CF_CREDENTIAL_VERBS
        for word in words
    ):
        return NO_CLOUDFLARE_RISK
    if first not in CF_DATA_RESOURCES:
        for index, word in enumerate(words):
            if word in CF_SECRET_RESOURCES or (
                CF_CREDENTIAL_WORD.search(word) and not cf_reads(words[index:])
            ):
                return NO_CLOUDFLARE_RISK
    for prefix in CF_MONEY_AND_MEMBERS:
        rest = cf_after(words, prefix)
        if rest is not None and not cf_reads(rest):
            return NO_CLOUDFLARE_RISK
    return None


def cf_after(words: list[str], path: tuple[str, ...]) -> list[str] | None:
    """The words after command path PATH, or None if the words lack it.

    The path starts at the first word that is not an option value, or at any
    word before it. Option values that cf_words keeps may stand between the
    command words.
    """
    for start, word in enumerate(words):
        if word == path[0]:
            rest = words[start + 1 :]
            for step in path[1:]:
                if step not in rest:
                    break
                rest = rest[rest.index(step) + 1 :]
            else:
                return rest
        if not isinstance(word, OptionValue):
            break
    return None


def cf_reads(words: list[str]) -> bool:
    """Whether the first verb among WORDS reads.

    A later argument named like a read verb cannot make a mutation look like a
    read. Only a word that no option could have taken can be a read verb, while
    a write verb counts wherever it stands, since an option that takes no value
    (-qq) leaves the next word a verb that the hook took for its value.
    """
    for word in words:
        if CF_WRITE_VERB.match(word):
            return False
        if not isinstance(word, OptionValue) and (
            word in CF_READ_VERBS or word.startswith(("get-", "list-"))
        ):
            return True
    return False


def command_reason(words: list[Word]) -> str | None:
    index = 0
    while index < len(words):
        word = words[index]
        executable = program(word)
        if ASSIGNMENT.match(word) or word in KEYWORDS or isinstance(word, Substitution):
            index += 1
        elif executable == "function":
            index += 2
        elif executable in WRAPPERS:
            index += 1
            while index < len(words) and words[index].startswith("-"):
                option = words[index]
                if executable == "command" and option in {"-v", "-V"}:
                    return None
                index += 2 if takes_value(executable, option) else 1
            if executable in COUNTED:
                index += 1
        else:
            break
    if index >= len(words):
        return None
    executable = program(words[index])
    args = words[index + 1 :]
    if executable == "git":
        return git_reason(args)
    if executable == "gh":
        return gh_reason(args)
    if executable in {"cf", "cloudflare"}:
        return cf_reason(args)
    if executable in {"bash", "zsh", "sh"}:
        script = nested_script(executable, args)
        if script is not None:
            return shell_reason(script, exact=script.level is Level.LITERAL)
    return None


def shell_reason(script: str, *, exact: bool = True) -> str | None:
    for words in commands(script):
        if not exact:
            # The shell that runs this script may read other text than this.
            words = [type(word)(word, Level.UNKNOWN) for word in words]  # noqa: PLW2901
        reason = command_reason(words)
        if reason:
            return reason
    return None


class OutOfTime(Exception):  # noqa: N818 - not an error condition in the usual sense
    pass


def out_of_time(_signum: int, _frame: FrameType | None) -> None:
    raise OutOfTime


def main() -> None:
    signal.signal(signal.SIGALRM, out_of_time)
    try:
        signal.setitimer(signal.ITIMER_REAL, WALL_BUDGET)
        try:
            payload = json.load(sys.stdin)
            tool = payload.get("tool_name")
            if tool not in SHELL_TOOLS:
                return
            tool_input = payload["tool_input"]
            # A Monitor WebSocket watch has `ws` in place of `command`.
            if (
                tool == "Monitor"
                and not tool_input.get("command")
                and tool_input.get("ws")
            ):
                return
            reason = shell_reason(tool_input["command"])
        finally:
            signal.setitimer(signal.ITIMER_REAL, 0)
    except Exception as exc:  # noqa: BLE001 - hook errors otherwise fail open
        reason = (
            f"Command policy check failed ({type(exc).__name__}). "
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
