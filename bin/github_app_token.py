#!/usr/bin/env python3

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

"""Keep a GitHub App user token fresh in a throwaway VM (Linux only).

bin/limavm runs the device flow on the Mac and sends the result here through
bin/setup-secrets. The access token lasts 8 hours and the refresh token 6 months,
and GitHub rotates the refresh token on every use, so each refresh writes the
new pair to ~/.config/github-app/auth.json before anything else sees the new
access token, and an exclusive lock keeps two callers from spending one refresh
token. A systemd timer (provision/throwaway.sh) runs refresh-if-needed, and the
git credential helper below refreshes on demand. After a refresh gh is logged in
again with the new access token, because the old one stops working at once.

This must run on the system Python of Debian 13 (3.13) as well as Homebrew's,
which a PATH without Homebrew first selects. ruff format rewrites
"except (A, B):" as "except A, B:", which only Python 3.14 parses, so catch
one exception type or use "as". No token is ever printed, except by
"credential get" to git.
"""

from __future__ import annotations

import argparse
import contextlib
import fcntl
import hashlib
import json
import os
import re
import shlex
import shutil
import socket
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import asdict, dataclass, replace
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, NoReturn, cast

if TYPE_CHECKING:
    from collections.abc import Callable, Generator

DEFAULT_WEB_URL = "https://github.com"
DEFAULT_API_URL = "https://api.github.com"
USER_AGENT = "dotfiles-github-app-token"
MARGIN = 600
LOCK_TIMEOUT = 120
HTTP_TIMEOUT = 30
GH_TIMEOUT = 120
GIT_TIMEOUT = 60
USERNAME = "x-access-token"
REPOSITORY = re.compile(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+")
TOKEN = re.compile(r"[A-Za-z0-9_.~+/=-]+")
CLIENT_ID = re.compile(r"[A-Za-z0-9._-]+")
AUTHORIZATIONS = "https://github.com/settings/apps/authorizations"
GH_ENVIRONMENT = (
    "GH_TOKEN",
    "GITHUB_TOKEN",
    "GH_ENTERPRISE_TOKEN",
    "GITHUB_ENTERPRISE_TOKEN",
    "GH_HOST",
)


class TokenError(Exception):
    """A failure whose message is safe to print: it never holds a token."""


def fail(message: str) -> NoReturn:
    raise TokenError(message)


def sentence(*parts: str) -> str:
    return " ".join(parts)


@dataclass(frozen=True)
class Record:
    client_id: str
    repository: str
    access_token: str
    access_expires_at: int
    refresh_token: str
    refresh_expires_at: int
    web_url: str = DEFAULT_WEB_URL
    api_url: str = DEFAULT_API_URL


def directory() -> Path:
    return Path.home() / ".config" / "github-app"


def reauthorize(repository: str) -> str:
    host = socket.gethostname()
    name = host.removeprefix("lima-") if host.startswith("lima-") else "NAME"
    return f"limavm github {name} {repository}"


def clean_url(value: object, field: str) -> str:
    if not isinstance(value, str):
        fail(f"{field} is not a string")
    parts = urllib.parse.urlsplit(value)
    if (
        parts.scheme not in {"http", "https"}
        or not parts.hostname
        or parts.username
        or parts.password
        or parts.query
        or parts.fragment
        or parts.path not in {"", "/", "/api/v3"}
    ):
        fail(f"{field} is not a base URL of GitHub")
    return value.rstrip("/")


def clean_token(value: object, field: str) -> str:
    if not isinstance(value, str) or TOKEN.fullmatch(value) is None:
        fail(f"{field} is not a token")
    return value


def clean_time(value: object, field: str) -> int:
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        fail(f"{field} is not a time in seconds since 1970")
    return value


def parse_record(text: str) -> Record:
    """The record from its JSON, checked; no message includes the text."""
    try:
        data = cast("object", json.loads(text))
    except ValueError as error:
        fail(f"the auth record is not valid JSON ({type(error).__name__})")
    if not isinstance(data, dict):
        fail("the auth record is not a JSON object")
    fields = cast("dict[str, object]", data)
    required = {
        "client_id",
        "repository",
        "access_token",
        "access_expires_at",
        "refresh_token",
        "refresh_expires_at",
    }
    missing = sorted(required - fields.keys())
    if missing:
        fail(f"the auth record lacks {', '.join(missing)}")
    unknown = sorted(fields.keys() - required - {"web_url", "api_url"})
    if unknown:
        fail(f"the auth record has unknown fields: {', '.join(unknown)}")
    client_id = fields["client_id"]
    if not isinstance(client_id, str) or CLIENT_ID.fullmatch(client_id) is None:
        fail("client_id is not a client ID")
    repository = fields["repository"]
    if (
        not isinstance(repository, str)
        or REPOSITORY.fullmatch(repository) is None
        or any(part in {".", ".."} for part in repository.split("/"))
    ):
        fail("repository is not OWNER/REPO")
    return Record(
        client_id=client_id,
        repository=repository,
        access_token=clean_token(fields["access_token"], "access_token"),
        access_expires_at=clean_time(fields["access_expires_at"], "access_expires_at"),
        refresh_token=clean_token(fields["refresh_token"], "refresh_token"),
        refresh_expires_at=clean_time(
            fields["refresh_expires_at"], "refresh_expires_at"
        ),
        web_url=clean_url(fields.get("web_url", DEFAULT_WEB_URL), "web_url"),
        api_url=clean_url(fields.get("api_url", DEFAULT_API_URL), "api_url"),
    )


def ensure_directory(path: Path) -> None:
    path.mkdir(parents=True, mode=0o700, exist_ok=True)
    path.chmod(0o700)


def write_atomic(path: Path, text: str) -> None:
    """Write beside the destination and rename, so a reader sees all or none."""
    ensure_directory(path.parent)
    handle, name = tempfile.mkstemp(dir=path.parent, prefix=f".{path.name}.")
    try:
        with os.fdopen(handle, "w") as out:
            _ = out.write(text)
            out.flush()
            os.fsync(out.fileno())
        _ = Path(name).replace(path)
    except BaseException:
        with contextlib.suppress(FileNotFoundError):
            Path(name).unlink()
        raise
    folder = os.open(path.parent, os.O_RDONLY)
    try:
        os.fsync(folder)
    finally:
        os.close(folder)


def save(record: Record) -> None:
    text = json.dumps(asdict(record), indent=2) + "\n"
    write_atomic(directory() / "auth.json", text)


def load() -> Record:
    path = directory() / "auth.json"
    try:
        text = path.read_text()
    except FileNotFoundError:
        fail(
            sentence(
                f"{path} does not exist, so this machine has no GitHub authorization;",
                "run limavm github NAME OWNER/REPO on the Mac",
            )
        )
    return parse_record(text)


@contextlib.contextmanager
def locked() -> Generator[None]:
    ensure_directory(directory())
    descriptor = os.open(directory() / "lock", os.O_RDWR | os.O_CREAT, 0o600)
    try:
        deadline = time.monotonic() + LOCK_TIMEOUT
        while True:
            try:
                fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() > deadline:
                    fail("another refresh holds the lock and does not finish")
                time.sleep(0.05)
        yield
    finally:
        os.close(descriptor)


def fetch(request: urllib.request.Request) -> bytes:
    """The body of a response; urllib raises for an HTTP error status."""
    with urllib.request.urlopen(request, timeout=HTTP_TIMEOUT) as response:  # noqa: S310  # pyright: ignore[reportAny]
        return cast("bytes", response.read())  # pyright: ignore[reportAny]


def decode_object(body: bytes) -> dict[str, object] | None:
    try:
        reply = cast("object", json.loads(body))
    except ValueError:
        return None
    return cast("dict[str, object]", reply) if isinstance(reply, dict) else None


def post_form(url: str, form: dict[str, str]) -> dict[str, object]:
    request = urllib.request.Request(  # noqa: S310
        url,
        data=urllib.parse.urlencode(form).encode(),
        headers={"Accept": "application/json", "User-Agent": USER_AGENT},
    )
    try:
        body = fetch(request)
    except urllib.error.HTTPError as error:
        reply = decode_object(error.read())
        if reply is None or "error" not in reply:
            fail(f"GitHub answered HTTP {error.code}")
        return reply
    except OSError as error:
        reason = error.reason if isinstance(error, urllib.error.URLError) else error
        fail(f"cannot reach {url}: {type(reason).__name__}")
    reply = decode_object(body)
    if reply is None:
        fail("GitHub sent a reply that is not a JSON object")
    return reply


def reply_int(reply: dict[str, object], key: str) -> int:
    value = reply.get(key)
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        fail(f"GitHub's reply has no usable {key}")
    return value


def reply_token(reply: dict[str, object], key: str) -> str:
    value = reply.get(key)
    if not isinstance(value, str) or TOKEN.fullmatch(value) is None:
        fail(f"GitHub's reply has no usable {key}")
    return value


def stamp(seconds: int) -> str:
    return datetime.fromtimestamp(seconds, UTC).strftime("%Y-%m-%dT%H:%M:%SZ")


def rotate(record: Record) -> Record:
    """The caller holds the lock. The new pair is saved before it is returned."""
    started = int(time.time())
    command = reauthorize(record.repository)
    if record.refresh_expires_at <= started:
        expired = stamp(record.refresh_expires_at)
        fail(
            sentence(
                f"the refresh token for {record.repository} expired at {expired};",
                f"authorize again with: {command}",
            )
        )
    reply = post_form(
        f"{record.web_url}/login/oauth/access_token",
        {
            "client_id": record.client_id,
            "grant_type": "refresh_token",
            "refresh_token": record.refresh_token,
        },
    )
    error = reply.get("error")
    if error == "bad_refresh_token":
        fail(
            sentence(
                f"GitHub rejected the refresh token for {record.repository}",
                "(bad_refresh_token): it expired, or it was already used.",
                "Refresh tokens rotate, so if this VM did not use it, a copy of it",
                "may have been used elsewhere.",
                f"Authorize again with: {command}.",
                "If you suspect a leak, de-authorize the app first at",
                f"{AUTHORIZATIONS}, which invalidates every token the app has issued.",
            )
        )
    if isinstance(error, str):
        description = reply.get("error_description")
        detail = f": {description}" if isinstance(description, str) else ""
        fail(f"GitHub refused the refresh ({error[:60]}{detail[:200]})")
    renewed = replace(
        record,
        access_token=reply_token(reply, "access_token"),
        access_expires_at=started + reply_int(reply, "expires_in"),
        refresh_token=reply_token(reply, "refresh_token"),
        refresh_expires_at=started + reply_int(reply, "refresh_token_expires_in"),
    )
    save(renewed)
    return renewed


def host_of(record: Record) -> str:
    return urllib.parse.urlsplit(record.web_url).netloc


def synced_path() -> Path:
    return directory() / "gh-synced"


def fingerprint(record: Record) -> str:
    return hashlib.sha256(record.access_token.encode()).hexdigest()


def is_synced(record: Record) -> bool:
    """Whether gh holds this access token. The file keeps a hash, so that it
    holds nothing secret and still tells two tokens with one expiry apart."""
    try:
        return synced_path().read_text().strip() == fingerprint(record)
    except FileNotFoundError:
        return False


def sync_gh(record: Record) -> None:
    """Log gh in with the current access token.

    gh keeps the token in ~/.config/gh/hosts.yml, and GH_HOST (unlike
    --hostname) accepts a host with a port.
    """
    gh = shutil.which("gh")
    if gh is None:
        fail("gh is not installed")
    environment = {
        key: value for key, value in os.environ.items() if key not in GH_ENVIRONMENT
    }
    environment.update(
        GH_HOST=host_of(record),
        GH_PROMPT_DISABLED="1",
        GH_NO_UPDATE_NOTIFIER="1",
        GH_TELEMETRY="false",
        NO_COLOR="1",
    )
    try:
        result = subprocess.run(
            [gh, "auth", "login", "--with-token", "--insecure-storage"],
            input=record.access_token + "\n",
            capture_output=True,
            text=True,
            check=False,
            env=environment,
            timeout=GH_TIMEOUT,
        )
    except (OSError, subprocess.TimeoutExpired) as error:
        fail(f"cannot run gh auth login: {type(error).__name__}")
    if result.returncode != 0:
        lines = result.stderr.replace(record.access_token, "***").strip().splitlines()
        fail(f"gh auth login failed: {lines[-1][:200] if lines else result.returncode}")
    write_atomic(synced_path(), f"{fingerprint(record)}\n")


def needs_refresh(record: Record, margin: int) -> bool:
    return record.access_expires_at - int(time.time()) < margin


def fresh_locked(margin: int, *, force: bool) -> tuple[Record, bool]:
    record = load()
    renewed = force or needs_refresh(record, margin)
    if renewed:
        record = rotate(record)
    if not is_synced(record):
        sync_gh(record)
    return record, renewed


def ensure_fresh(margin: int = MARGIN, *, force: bool = False) -> tuple[Record, bool]:
    """The lock is taken only when work is due, so a fresh token costs two reads."""
    if not force:
        record = load()
        if not needs_refresh(record, margin) and is_synced(record):
            return record, False
    with locked():
        return fresh_locked(margin, force=force)


def configure_git(record: Record) -> None:
    git = shutil.which("git")
    if git is None:
        fail("git is not installed")
    config = Path.home() / ".config" / "git" / "config"
    config.parent.mkdir(parents=True, exist_ok=True)
    section = f"credential.{record.web_url}"
    helper = f"!{shlex.quote(str(Path(__file__).resolve()))} credential"
    for key, value in (
        (f"{section}.helper", helper),
        (f"{section}.useHttpPath", "true"),
    ):
        result = subprocess.run(
            [git, "config", "--file", str(config), "--replace-all", key, value],
            capture_output=True,
            text=True,
            check=False,
            timeout=GIT_TIMEOUT,
        )
        if result.returncode != 0:
            fail(f"git config {key} failed: {result.stderr.strip()[:200]}")


class Arguments(argparse.Namespace):
    command: str = ""
    margin: int = MARGIN
    force: bool = False
    operation: str = ""
    check: bool = False


def command_install(_arguments: Arguments) -> int:
    record = parse_record(sys.stdin.read())
    with locked():
        save(record)
        synced_path().unlink(missing_ok=True)
        configure_git(record)
        _ = fresh_locked(MARGIN, force=False)
    return 0


def command_refresh(arguments: Arguments) -> int:
    record, renewed = ensure_fresh(arguments.margin, force=arguments.force)
    if renewed:
        print(
            f"github_app_token: renewed the token for {record.repository};",
            f"it lasts until {stamp(record.access_expires_at)}",
        )
    return 0


def read_attributes() -> dict[str, str]:
    attributes: dict[str, str] = {}
    for line in sys.stdin.read().splitlines():
        if not line:
            break
        key, separator, value = line.partition("=")
        if separator:
            attributes[key] = value
    return attributes


def repository_path(path: str) -> str:
    return path.strip("/").removesuffix(".git").lower()


def command_credential(arguments: Arguments) -> int:
    attributes = read_attributes()
    if arguments.operation != "get":
        return 0
    record = load()
    web = urllib.parse.urlsplit(record.web_url)
    if attributes.get("protocol") != web.scheme or attributes.get("host") != web.netloc:
        return 0
    if repository_path(attributes.get("path", "")) != record.repository.lower():
        return 0
    record, _ = ensure_fresh()
    lines = [
        f"username={USERNAME}",
        f"password={record.access_token}",
        f"password_expiry_utc={record.access_expires_at}",
    ]
    _ = sys.stdout.write("\n".join(lines) + "\n")
    return 0


def remaining(seconds: int) -> str:
    left = seconds - int(time.time())
    if left < 0:
        return f"expired {-left // 60} minutes ago"
    return f"in {left // 3600}h{left % 3600 // 60:02d}m"


def command_status(arguments: Arguments) -> int:
    record = load()
    synced = is_synced(record)
    print(f"repository: {record.repository}")
    print(f"web: {record.web_url}  api: {record.api_url}")
    access, refresh = record.access_expires_at, record.refresh_expires_at
    print(f"access token expires {stamp(access)} ({remaining(access)})")
    print(f"refresh token expires {stamp(refresh)} ({remaining(refresh)})")
    print(f"gh is logged in with the current access token: {'yes' if synced else 'no'}")
    if not arguments.check:
        return 0
    request = urllib.request.Request(  # noqa: S310
        f"{record.api_url}/repos/{record.repository}",
        headers={
            "Accept": "application/vnd.github+json",
            "Authorization": f"Bearer {record.access_token}",
            "User-Agent": USER_AGENT,
        },
    )
    try:
        reply = decode_object(fetch(request))
    except urllib.error.HTTPError as error:
        print(f"api: HTTP {error.code}")
        return 1
    except OSError as error:
        fail(f"cannot check the token: {type(error).__name__}")
    if reply is None:
        fail("the API sent a reply that is not a JSON object")
    permissions = reply.get("permissions")
    push = (
        cast("dict[str, object]", permissions).get("push")
        if isinstance(permissions, dict)
        else None
    )
    print(f"api: HTTP 200, push={push}")
    return 0 if push is True else 1


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Keep a GitHub App user token fresh.")
    commands = parser.add_subparsers(dest="command", required=True)
    _ = commands.add_parser(
        "install", help="read the auth record from stdin and set everything up"
    )
    refresh = commands.add_parser(
        "refresh-if-needed", help="renew the access token when it has little time left"
    )
    _ = refresh.add_argument(
        "--force", action="store_true", help="renew it even if it is fresh"
    )
    _ = refresh.add_argument(
        "--margin",
        type=int,
        default=MARGIN,
        help="seconds of life below which to renew",
    )
    credential = commands.add_parser("credential", help="git credential helper")
    _ = credential.add_argument("operation", choices=["get", "store", "erase"])
    status = commands.add_parser("status", help="show the expiry times, without tokens")
    _ = status.add_argument(
        "--check",
        action="store_true",
        help="also ask the API whether the token can push",
    )
    return parser


COMMANDS: dict[str, Callable[[Arguments], int]] = {
    "install": command_install,
    "refresh-if-needed": command_refresh,
    "credential": command_credential,
    "status": command_status,
}


def main(argv: list[str]) -> int:
    arguments = build_parser().parse_args(argv, namespace=Arguments())
    try:
        return COMMANDS[arguments.command](arguments)
    except TokenError as error:
        print(f"github_app_token: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
