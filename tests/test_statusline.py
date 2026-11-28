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

# Exercises .claude/statusline.sh end to end: each case feeds it the JSON
# Claude Code sends and checks the rendered line. The script runs the way
# Claude Code runs it — by path, under its own #!/bin/bash — with starship
# and the clock shimmed so the left segment and every countdown are exact,
# and curl and the keychain shimmed so the usage and pool fetches can be driven
# through their success and failure paths without reaching the network.

import json
import os
import shutil
import subprocess
import time
from pathlib import Path
from typing import final

import pytest

REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / ".claude" / "statusline.sh"
BOLD = "\x1b[1m"
NOBOLD = "\x1b[22m"
HOUR = 3600
FIVE_HOURS = 5 * HOUR
WEEK = 7 * 24 * HOUR
# The script refetches usage once its cache is this old.
CACHE_TTL = 600
# The clock the script sees. It must stay close to real time so a cache
# written moments ago counts as fresh.
NOW = int(time.time())
# What the starship shim's colored, padded output must be reduced to.
LEFT = "main (abc1234) [!]"
FABLE_USAGE = {
    "limits": [{"scope": {"model": {"display_name": "Fable"}}, "percent": 45.7}]
}
TOKEN = "test-oauth-token"  # noqa: S105 -- a fake, checked only for placement
CREDENTIALS = {"claudeAiOauth": {"accessToken": TOKEN}}

STARSHIP_SHIM = r"""
[[ -n "${STATUSLINE_TEST_STARSHIP_FAIL:-}" ]] && exit 1
case "$2" in
    git_branch) printf ' \033[1;35m main\033[0m ' ;;
    git_commit) printf '\033[32m(abc1234)\033[0m ' ;;
    git_status) printf '\033[31m[!]\033[0m ' ;;
    *) exit 1 ;;
esac
"""

# Records the call, the URL, the argv, the proxy variable and the headers
# (including any read from an @file, as curl would), then answers as
# STATUSLINE_TEST_CURL directs, or STATUSLINE_TEST_POOL_CURL for the pool path:
# a body written to the -o file, or one of curl's failure exit codes.
CURL_SHIM = r"""
log="$STATUSLINE_TEST_LOG"
printf '%s\n' curl >> "$log/calls"
for url; do :; done
printf '%s\n' "$url" >> "$log/urls"
mode="${STATUSLINE_TEST_CURL:-}"
response="${STATUSLINE_TEST_RESPONSE:-}"
if [[ "$url" == */__cog__/pool ]]; then
    mode="${STATUSLINE_TEST_POOL_CURL:-}"
    response="${STATUSLINE_TEST_POOL_RESPONSE:-}"
fi
printf '%s\n' "$*" > "$log/curl.argv"
printf '%s' "${HTTPS_PROXY-unset}" > "$log/curl.proxy"
: > "$log/curl.headers"
out=''
while (($#)); do
    case "$1" in
        -H)
            shift
            if [[ "$1" == @* ]]; then
                cat "${1#@}" >> "$log/curl.headers"
            else
                printf '%s\n' "$1" >> "$log/curl.headers"
            fi
            ;;
        -o)
            shift
            out="$1"
            ;;
    esac
    shift
done
case "$mode" in
    ok) printf '%s' "$response" > "$out" ;;
    garbage) printf 'not json' > "$out" ;;
    http-error) exit 22 ;;
    noisy-http-error) echo 'curl: (22) error: 503' >&2; exit 22 ;;
    timeout) exit 28 ;;
    *) exit 1 ;;
esac
"""

# Prints the keychain item when the test provides one; otherwise fails the
# way `security` does when the item is missing.
SECURITY_SHIM = r"""
printf '%s\n' security >> "$STATUSLINE_TEST_LOG/calls"
[[ -n "${STATUSLINE_TEST_KEYCHAIN:-}" ]] || exit 44
printf '%s\n' "$STATUSLINE_TEST_KEYCHAIN"
"""


def window(used: object, left: float) -> dict[str, object]:
    """A rate-limit window `used` percent spent, resetting `left` seconds from now."""
    return {"used_percentage": used, "resets_at": NOW + left}


# A subscriber session 100 hours before the weekly reset, so 40% of the week
# has elapsed. The Fable segment only appears in such a session.
SUBSCRIBER = {"rate_limits": {"seven_day": window(40, 100 * HOUR)}}
# FABLE_USAGE's 45.7%, floored, is ahead of that pace.
FABLE_SEGMENT = f"Fable {BOLD}55%{NOBOLD}"


# A made-up proxy URL, whose key must appear nowhere.
PROXY_KEY = "k" * 43
PROXY = f"http://cog:{PROXY_KEY}@[::1]:17443"
POOL_URL = "https://api.anthropic.com/__cog__/pool"
USAGE_URL = "https://api.anthropic.com/api/oauth/usage"
# The gateway's clock, years from the one the script sees, so any duration
# taken from the local clock is visibly wrong.
POOL_NOW = 1_000_000_000_000


def bucket(name: str, used: float, left: float) -> dict[str, object]:
    return {
        "bucket": name,
        "utilization": used,
        "expires": POOL_NOW + round(left * 1000),
    }


def account(
    name: str,
    *buckets: dict[str, object],
    held: float | None = None,
    hold: str = "limit",
) -> dict[str, object]:
    return {
        "name": name,
        "held": held is not None,
        "heldUntil": None if held is None else POOL_NOW + round(held * 1000),
        "hold": None if held is None else hold,
        "buckets": list(buckets),
    }


def pool_answer(
    *accounts: dict[str, object], current: str | None = None, now: float = POOL_NOW
) -> dict[str, object]:
    return {
        "state": "usable",
        "now": now,
        "current": current,
        "accounts": list(accounts),
    }


def shim(target: Path, body: str) -> None:
    _ = target.write_text(f"#!/bin/bash\n{body}")
    target.chmod(0o755)


@final
class Statusline:
    def __init__(self, home: Path) -> None:
        self.home = home
        self.usage = home / "cache" / "claude-code-statusline" / "usage.json"
        self.usage.parent.mkdir(parents=True)
        self.pool = self.usage.parent / "pool.json"
        self.log = home / "log"
        self.log.mkdir()
        shims = home / "shims"
        shims.mkdir()
        self.env = dict(os.environ)
        for name in ("CLAUDE_CODE_OAUTH_TOKEN", "HTTPS_PROXY", "https_proxy"):
            _ = self.env.pop(name, None)
        self.env.update(
            PATH=f"{shims}:{os.environ['PATH']}",
            HOME=str(home),
            XDG_CACHE_HOME=str(home / "cache"),
            STATUSLINE_TEST_LOG=str(self.log),
        )
        shim(shims / "starship", STARSHIP_SHIM)
        shim(shims / "curl", CURL_SHIM)
        shim(shims / "security", SECURITY_SHIM)
        # A file that the script writes is stamped by the real clock, which is
        # never earlier than the frozen one; cap it so that it reads as fresh.
        shim(
            shims / "date",
            rf"""
if [[ $# -eq 1 && "$1" == "+%s" ]]; then
    printf '%s\n' {NOW}
elif [[ $# -eq 3 && "$1" == "-r" && "$3" == "+%s" ]] &&
    [[ -z "${{STATUSLINE_TEST_REAL_MTIME:-}}" ]]; then
    mtime=$(/bin/date "$@") || exit
    printf '%s\n' $((mtime > {NOW} ? {NOW} : mtime))
else
    exec /bin/date "$@"
fi
""",
        )

    def cache(self, usage: object, *, age: float = 0) -> None:
        """Seed the usage cache, `age` seconds old by the script's clock."""
        _ = self.usage.write_text(json.dumps(usage))
        os.utime(self.usage, (NOW - age, NOW - age))

    def seed_pool(self, answer: object, *, age: float = 0) -> None:
        _ = self.pool.write_text(json.dumps(answer))
        os.utime(self.pool, (NOW - age, NOW - age))

    def pool_curl(
        self, mode: str, response: object = None, *, raw: str | None = None
    ) -> None:
        self.env["STATUSLINE_TEST_POOL_CURL"] = mode
        self.env["STATUSLINE_TEST_POOL_RESPONSE"] = (
            json.dumps(response) if raw is None else raw
        )

    def mac_pooled(self, proxy: str = PROXY) -> None:
        self.env["OSTYPE"] = "darwin25.0"
        self.env["HTTPS_PROXY"] = proxy

    def linux_pooled(self, token: str = "cog\n") -> None:  # noqa: S107 -- placeholder
        marker = self.home / "cog-gateway"
        _ = marker.write_text("cloudflare\n")
        secret = self.home / ".config" / "secrets" / "CLAUDE_CODE_OAUTH_TOKEN"
        secret.parent.mkdir(parents=True)
        _ = secret.write_text(token)
        self.env["OSTYPE"] = "linux-gnu"
        self.env["DOTFILES_GATEWAY_MARKER"] = str(marker)

    def urls(self) -> list[str]:
        log = self.log / "urls"
        return log.read_text().split() if log.exists() else []

    def credentials(self, content: object) -> None:
        path = self.home / ".claude" / ".credentials.json"
        path.parent.mkdir()
        _ = path.write_text(json.dumps(content))

    def curl(self, mode: str, response: object = None) -> None:
        """Have the curl shim answer with `response`, or fail as `mode` says."""
        self.env["STATUSLINE_TEST_CURL"] = mode
        self.env["STATUSLINE_TEST_RESPONSE"] = json.dumps(response)

    def calls(self) -> list[str]:
        """The fetch commands the script ran, in order."""
        log = self.log / "calls"
        return log.read_text().split() if log.exists() else []

    def headers(self) -> list[str]:
        return (self.log / "curl.headers").read_text().splitlines()

    def run(self, payload: object = None) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [str(SCRIPT)],
            input="" if payload is None else json.dumps(payload),
            capture_output=True,
            text=True,
            env=self.env,
            cwd=self.home,
            check=False,
        )

    def line(self, payload: object = None) -> str:
        result = self.run(payload)
        assert result.returncode == 0, result.stderr
        assert result.stderr == ""
        return result.stdout

    def render(self, payload: object = None, *, usage: object = None) -> str:
        """Render against a fresh cache holding `usage`, so nothing is fetched."""
        self.cache({} if usage is None else usage)
        line = self.line(payload)
        assert self.calls() == [], "the script tried to fetch usage"
        return line


@pytest.fixture
def statusline(tmp_path: Path) -> Statusline:
    return Statusline(tmp_path)


def test_subscription_session(statusline: Statusline) -> None:
    line = statusline.render(
        {
            "model": {"display_name": "Fable 5.1"},
            "effort": {"level": "high"},
            "cost": {"total_cost_usd": 1.234},
            "rate_limits": {
                "five_hour": window(13, 4 * HOUR + 32 * 60 + 30),
                "seven_day": window(38, 42 * HOUR + 30 * 60),
            },
        },
        usage=FABLE_USAGE,
    )
    # 13% used 27 minutes into the five-hour window is ahead of pace, so
    # bold; 38% used 125 hours into the week is not. The cost stays hidden
    # until the week is used up.
    right = f"4:32 {BOLD}87%{NOBOLD} · 42h 62% · Fable 55%"
    assert line == f" {LEFT} · Fable 5.1 high · {right}"


@pytest.mark.parametrize(
    ("left", "label"),
    [
        (WEEK + HOUR, "168h"),  # clock skew past the reset is clamped
        (WEEK, "168h"),
        (WEEK - 1, "167h"),
        (42 * HOUR, "42h"),
        (42 * HOUR - 1, "41h"),
        (HOUR, "1h"),
        (HOUR - 1, "0h"),
        (0, "0h"),
        (-HOUR, "0h"),  # a reset that has already passed
    ],
)
def test_week_label_counts_down_whole_hours(
    statusline: Statusline, left: int, label: str
) -> None:
    line = statusline.render({"rate_limits": {"seven_day": window(0, left)}})
    assert line == f" {LEFT} · {label} 100% · Fable ??"


@pytest.mark.parametrize(
    "limit",
    [{"used_percentage": 38}, {"used_percentage": 38, "resets_at": "soon"}],
)
def test_week_label_falls_back_without_reset_time(
    statusline: Statusline, limit: dict[str, object]
) -> None:
    line = statusline.render({"rate_limits": {"seven_day": limit}})
    assert line == f" {LEFT} · week 62%"


@pytest.mark.parametrize(
    ("left", "clock"),
    [
        (FIVE_HOURS + 5, "5:00"),
        (FIVE_HOURS, "5:00"),
        (FIVE_HOURS - 1, "4:59"),
        (61, "0:01"),
        (59, "0:00"),
        (0, "0:00"),
        (-5, "0:00"),
    ],
)
def test_five_hour_clock_counts_down(
    statusline: Statusline, left: int, clock: str
) -> None:
    line = statusline.render({"rate_limits": {"five_hour": window(0, left)}})
    assert line == f" {LEFT} · {clock} 100% · Fable ??"


@pytest.mark.parametrize(("used", "segment"), [(60, f"{BOLD}40%{NOBOLD}"), (40, "60%")])
def test_bold_marks_usage_ahead_of_pace(
    statusline: Statusline, used: int, segment: str
) -> None:
    # 100 hours left means 68 hours elapsed, 40% of the week.
    line = statusline.render({"rate_limits": {"seven_day": window(used, 100 * HOUR)}})
    assert line == f" {LEFT} · 100h {segment} · Fable ??"


def test_fable_shares_the_weekly_pace(statusline: Statusline) -> None:
    line = statusline.render(SUBSCRIBER, usage=FABLE_USAGE)
    assert line == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"


@pytest.mark.parametrize(
    "usage",
    [
        {"limits": [{"scope": {"model": {"display_name": "Opus"}}, "percent": 10}]},
        {},  # a cached fetch failure
        {"limits": "unexpected"},
    ],
)
def test_fable_shows_unknown_without_a_cached_figure(
    statusline: Statusline, usage: object
) -> None:
    assert (
        statusline.render(SUBSCRIBER, usage=usage) == f" {LEFT} · 100h 60% · Fable ??"
    )


def test_fable_needs_a_subscriber_session(statusline: Statusline) -> None:
    payload = {"model": {"display_name": "Fable 5.1"}}
    assert statusline.render(payload, usage=FABLE_USAGE) == f" {LEFT} · Fable 5.1"


def test_api_key_session_always_shows_cost(statusline: Statusline) -> None:
    line = statusline.render(
        {
            "model": {"display_name": "Opus 5 (1M context)"},
            "cost": {"total_cost_usd": 1.234},
        }
    )
    assert line == f" {LEFT} · Opus 5 (1M) · $1.23"
    assert statusline.render({"cost": {"total_cost_usd": 0}}) == f" {LEFT}"


@pytest.mark.parametrize(
    ("used", "tail"),
    [
        (99, f"{BOLD}1%{NOBOLD} · Fable ??"),
        (100, f"{BOLD}0%{NOBOLD} · Fable ?? · $0.50"),
    ],
)
def test_subscription_shows_cost_once_the_week_is_used_up(
    statusline: Statusline, used: int, tail: str
) -> None:
    line = statusline.render(
        {
            "cost": {"total_cost_usd": 0.5},
            "rate_limits": {"seven_day": window(used, 10 * HOUR)},
        }
    )
    assert line == f" {LEFT} · 10h {tail}"


def test_json_values_are_normalized(statusline: Statusline) -> None:
    # Percentages round, reset times floor, and wrong-typed fields are
    # treated as absent rather than aborting the line.
    line = statusline.render(
        {
            "model": {"display_name": 5},
            "effort": {"level": "high"},
            "rate_limits": {
                "five_hour": window("13", 59 * 60 + 59.9),
                "seven_day": window(37.6, 42 * HOUR + 0.9),
            },
        }
    )
    assert line == f" {LEFT} · 0:59 · 42h 62% · Fable ??"


@pytest.mark.parametrize("payload", [None, {}])
def test_empty_input(statusline: Statusline, payload: object) -> None:
    assert statusline.render(payload) == f" {LEFT}"


def test_starship_failure_aborts(statusline: Statusline) -> None:
    statusline.env["STATUSLINE_TEST_STARSHIP_FAIL"] = "1"
    result = statusline.run({"model": {"display_name": "Fable 5.1"}})
    assert result.returncode != 0
    assert result.stdout == ""


def test_first_run_fetches_usage(statusline: Statusline) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    shutil.rmtree(statusline.usage.parent)  # not even the cache directory yet
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert json.loads(statusline.usage.read_text()) == FABLE_USAGE
    assert not list(statusline.usage.parent.glob("usage.json.*")), "temp file left"
    # The credentials file supplied the token, so the keychain was not
    # consulted; the token travelled in a header file and never in argv.
    assert statusline.calls() == ["curl"]
    argv = (statusline.log / "curl.argv").read_text()
    assert "https://api.anthropic.com/api/oauth/usage" in argv
    assert TOKEN not in argv
    assert f"Authorization: Bearer {TOKEN}" in statusline.headers()
    assert "anthropic-beta: oauth-2025-04-20" in statusline.headers()


@pytest.mark.parametrize(
    ("proxy", "seen"),
    [
        ("http://corp.example:3128", "http://corp.example:3128"),
        (None, "unset"),
    ],
    ids=["another-proxy", "no-proxy"],
)
def test_usage_is_fetched_with_the_environments_proxy(
    statusline: Statusline, proxy: str | None, seen: str
) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    if proxy is not None:
        statusline.env["HTTPS_PROXY"] = proxy
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert (statusline.log / "curl.proxy").read_text() == seen


def test_the_environments_oauth_token_is_not_the_usage_token(
    statusline: Statusline,
) -> None:
    statusline.env["CLAUDE_CODE_OAUTH_TOKEN"] = "env-token"  # noqa: S105
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.calls() == ["curl"]
    assert f"Authorization: Bearer {TOKEN}" in statusline.headers()
    assert "env-token" not in "".join(statusline.headers())


@pytest.mark.parametrize(
    ("age", "refetched"), [(CACHE_TTL, True), (CACHE_TTL - 1, False)]
)
def test_cache_is_refetched_once_stale(
    statusline: Statusline, age: int, *, refetched: bool
) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.cache({}, age=age)
    segment = FABLE_SEGMENT if refetched else "Fable ??"
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {segment}"
    assert statusline.calls() == (["curl"] if refetched else [])


@pytest.mark.parametrize("ahead", [1, 2 * HOUR])
def test_a_usage_cache_dated_after_the_clock_is_refetched(
    statusline: Statusline, ahead: int
) -> None:
    statusline.env["STATUSLINE_TEST_REAL_MTIME"] = "1"
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.cache({}, age=-ahead)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.calls() == ["curl"]


def test_a_missing_cache_root_is_created_for_usage(statusline: Statusline) -> None:
    root = statusline.home / "new" / "cache"
    statusline.env["XDG_CACHE_HOME"] = str(root)
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert (root / "claude-code-statusline" / "usage.json").exists()


def seed_temporaries(statusline: Statusline) -> tuple[list[Path], list[Path]]:
    directory = statusline.usage.parent
    hour_ago = time.time() - HOUR
    minutes_ago = time.time() - 3 * 60
    stale = [directory / "pool.json.AbCdEf", directory / "usage.json.GhIjKl"]
    old_kept = [directory / "pool.json.TooLong1", directory / "notes.txt"]
    recent = directory / "pool.json.MnOpQr"
    for path in [*stale, *old_kept]:
        _ = path.write_text("")
        os.utime(path, (hour_ago, hour_ago))
    _ = recent.write_text("")
    os.utime(recent, (minutes_ago, minutes_ago))
    folder = directory / "folder.json.AbCdEf"
    folder.mkdir()
    os.utime(folder, (hour_ago, hour_ago))
    return stale, [recent, *old_kept, folder]


def test_stale_temporary_files_are_swept_when_usage_is_fetched(
    statusline: Statusline,
) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    stale, kept = seed_temporaries(statusline)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert [path for path in stale if path.exists()] == []
    assert all(path.exists() for path in kept)


@pytest.mark.parametrize(
    ("ostype", "default_cache"),
    [("darwin25.0", "Library/Caches"), ("linux-gnu", ".cache")],
)
def test_default_cache_directory_depends_on_the_os(
    statusline: Statusline, ostype: str, default_cache: str
) -> None:
    del statusline.env["XDG_CACHE_HOME"]
    statusline.env["OSTYPE"] = ostype
    statusline.usage = (
        statusline.home / default_cache / "claude-code-statusline" / "usage.json"
    )
    statusline.usage.parent.mkdir(parents=True)
    assert statusline.render(SUBSCRIBER, usage=FABLE_USAGE) == (
        f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    )


def test_keychain_supplies_the_token_without_a_credentials_file(
    statusline: Statusline,
) -> None:
    statusline.env["STATUSLINE_TEST_KEYCHAIN"] = json.dumps(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.calls() == ["security", "curl"]
    assert f"Authorization: Bearer {TOKEN}" in statusline.headers()


def test_keychain_backs_a_credentials_file_without_a_token(
    statusline: Statusline,
) -> None:
    statusline.credentials({"claudeAiOauth": {}})
    statusline.env["STATUSLINE_TEST_KEYCHAIN"] = json.dumps(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.calls() == ["security", "curl"]


@pytest.mark.parametrize("env_token", [None, "env-token"])
def test_no_token_means_no_request(
    statusline: Statusline, env_token: str | None
) -> None:
    if env_token is not None:
        statusline.env["CLAUDE_CODE_OAUTH_TOKEN"] = env_token
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · Fable ??"
    assert statusline.calls() == ["security"]
    assert json.loads(statusline.usage.read_text()) == {}


@pytest.mark.parametrize("failure", ["http-error", "timeout", "garbage"])
def test_failed_fetch_is_cached_and_not_retried(
    statusline: Statusline, failure: str
) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl(failure)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · Fable ??"
    assert json.loads(statusline.usage.read_text()) == {}
    assert not list(statusline.usage.parent.glob("usage.json.*")), "temp file left"
    # The failure is cached for as long as a success would be.
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · Fable ??"
    assert statusline.calls() == ["curl"]


def test_api_key_session_never_fetches(statusline: Statusline) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    payload = {"model": {"display_name": "Fable 5.1"}}
    assert statusline.line(payload) == f" {LEFT} · Fable 5.1"
    assert statusline.calls() == []
    assert not statusline.usage.exists()


def test_unwritable_cache_directory_still_renders(statusline: Statusline) -> None:
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    shutil.rmtree(statusline.usage.parent)
    _ = statusline.usage.parent.write_text("")
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · Fable ??"
    assert statusline.calls() == []
    assert not statusline.usage.exists()


OWN_SEGMENTS = {
    "rate_limits": {
        "five_hour": window(13, 4 * HOUR + 32 * 60 + 30),
        "seven_day": window(38, 42 * HOUR + 30 * 60),
    }
}
OWN_TEXT = f"4:32 {BOLD}87%{NOBOLD} · 42h 62%"


def pooled(
    statusline: Statusline, *accounts: dict[str, object], current: str | None = None
) -> str:
    statusline.mac_pooled()
    statusline.seed_pool(pool_answer(*accounts, current=current))
    line = statusline.line()
    assert statusline.calls() == [], "the script fetched in a fresh cache"
    return line


def test_pooled_session_shows_each_account(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(
        pool_answer(
            account("shields-msrl-com", held=62 * HOUR + 60),
            account(
                "shields2-msrl-com",
                bucket("7d", 0.31, 141 * HOUR),
                bucket("7d_oi", 0.06, 141 * HOUR),
            ),
            current="shields2-msrl-com",
        )
    )
    line = statusline.line({"model": {"display_name": "Fable 5.2"}, **OWN_SEGMENTS})
    # 31% of the week is used 27 hours in, which is ahead of pace; the 6% of
    # the Fable window is behind it.
    assert line == (
        f" {LEFT} · Fable 5.2 · shields out 62h"
        f" · {BOLD}shields2{NOBOLD} {BOLD}69%{NOBOLD} 141h Fable 94%"
    )
    assert OWN_TEXT not in line
    assert statusline.urls() == []


@pytest.mark.parametrize(
    ("names", "labels"),
    [
        (["a-one", "b-two"], ["a", "b"]),
        (["a-one", "a-two"], ["a-one", "a-two"]),
        (["a-one", "b-two", "a-three"], ["a-one", "b-two", "a-three"]),
        (["solo"], ["solo"]),
        (["a", "a-b"], ["a", "a-b"]),
    ],
)
def test_names_are_short_unless_that_is_ambiguous(
    statusline: Statusline, names: list[str], labels: list[str]
) -> None:
    line = pooled(statusline, *(account(name) for name in names))
    assert line == f" {LEFT} · " + " · ".join(f"{label} ??" for label in labels)


@pytest.mark.parametrize("current", [None, "b-two", "elsewhere"])
def test_only_the_current_account_is_bold(
    statusline: Statusline, current: str | None
) -> None:
    line = pooled(statusline, account("a-one"), account("b-two"), current=current)
    names = {"a": "a-one", "b": "b-two"}
    expected = [
        f"{BOLD}{short}{NOBOLD} ??" if full == current else f"{short} ??"
        for short, full in names.items()
    ]
    assert line == f" {LEFT} · " + " · ".join(expected)


@pytest.mark.parametrize(
    ("seconds", "text"),
    [
        (62 * HOUR, "out 62h"),
        (62 * HOUR + 59 * 60, "out 62h"),
        (HOUR, "out 1h"),
        (HOUR - 1, "out 59m"),
        (HOUR - 0.1, "out 59m"),
        (HOUR + 0.9, "out 1h"),
        (42 * 60, "out 42m"),
        (60, "out 1m"),
        (59.9, "out 0m"),
        (59, "out 0m"),
        (1, "out 0m"),
    ],
)
def test_held_account_shows_the_time_left(
    statusline: Statusline, seconds: float, text: str
) -> None:
    # A held account's buckets are not shown.
    line = pooled(statusline, account("a", bucket("7d", 1, 10 * HOUR), held=seconds))
    assert line == f" {LEFT} · a {text}"


@pytest.mark.parametrize(
    ("seconds", "text"),
    [(10 * 60 - 1, "401 9m"), (10 * 60, "401 10m"), (2 * HOUR, "401 2h")],
)
def test_an_account_whose_token_was_refused_says_so(
    statusline: Statusline, seconds: int, text: str
) -> None:
    line = pooled(
        statusline,
        account("a", bucket("7d", 0.2, 10 * HOUR), held=seconds, hold="refused"),
    )
    assert line == f" {LEFT} · a {text}"


@pytest.mark.parametrize("held", [0, -HOUR])
def test_a_hold_that_has_ended_is_not_a_hold(statusline: Statusline, held: int) -> None:
    line = pooled(statusline, account("a", bucket("7d", 0.5, 10 * HOUR), held=held))
    assert line == f" {LEFT} · a 50% 10h"


def test_buckets_come_in_a_fixed_order_and_others_are_ignored(
    statusline: Statusline,
) -> None:
    line = pooled(
        statusline,
        account(
            "a",
            bucket("7d_oi", 0.06, 100 * HOUR),
            bucket("30d", 0.9, 100 * HOUR),
            bucket("7d", 0.3, 100 * HOUR),
            bucket("5h", 0.13, 2 * HOUR + 30 * 60),
        ),
    )
    assert line == f" {LEFT} · a 87% 2:30 70% 100h Fable 94%"


@pytest.mark.parametrize(
    ("buckets", "text"),
    [
        ([bucket("5h", 0.4, 90 * 60)], "60% 1:30"),
        ([bucket("7d", 0.4, 100 * HOUR)], "60% 100h"),
        ([bucket("7d_oi", 0.4, 100 * HOUR)], "Fable 60%"),
        ([bucket("30d", 0.4, 100 * HOUR)], "??"),
        ([], "??"),
        ([bucket("7d", 0.4, 0)], "??"),  # reset at the answer's now
        ([bucket("7d", 0.4, -HOUR)], "??"),
        ([bucket("7d", 0.4, -HOUR), bucket("5h", 0.4, 90 * 60)], "60% 1:30"),
    ],
)
def test_each_bucket_and_what_is_unknown(
    statusline: Statusline, buckets: list[dict[str, object]], text: str
) -> None:
    line = pooled(statusline, account("a", *buckets))
    # Every bucket here is behind pace, so none is bold.
    assert line == f" {LEFT} · a {text}"


@pytest.mark.parametrize(
    ("used", "text"),
    [
        (0, "100%"),
        (0.005, "99%"),
        (0.394, "61%"),
        (0.994, f"{BOLD}1%{NOBOLD}"),
        (0.995, f"{BOLD}0%{NOBOLD}"),
        (1, f"{BOLD}0%{NOBOLD}"),
        (2.5, f"{BOLD}0%{NOBOLD}"),
        (-0.5, "100%"),
    ],
)
def test_remaining_percent_is_rounded_and_clamped(
    statusline: Statusline, used: float, text: str
) -> None:
    # 100 hours left means 40% of the week has gone; bold only if used more.
    line = pooled(statusline, account("a", bucket("7d", used, 100 * HOUR)))
    assert line == f" {LEFT} · a {text} 100h"


@pytest.mark.parametrize(
    ("left", "clock", "hours"),
    [
        (FIVE_HOURS + 100, "5:00", None),
        (FIVE_HOURS, "5:00", None),
        (FIVE_HOURS - 1, "4:59", None),
        (11 * 60, "0:11", None),
        (10 * 60, "0:10", None),
        (9 * 60, "0:09", None),
        (61, "0:01", None),
        (59.9, "0:00", None),
        (59, "0:00", None),
        (WEEK + HOUR, None, "168h"),
        (WEEK - 1, None, "167h"),
        (HOUR - 0.1, None, "0h"),
        (HOUR - 1, None, "0h"),
    ],
)
def test_times_left_are_clamped_and_floored(
    statusline: Statusline, left: float, clock: str | None, hours: str | None
) -> None:
    name = "5h" if clock else "7d"
    line = pooled(statusline, account("a", bucket(name, 0, left)))
    assert line == f" {LEFT} · a 100% {clock or hours}"


@pytest.mark.parametrize(
    ("name", "left", "text"),
    [
        # Halfway through the window, 60% used is ahead of pace and 40% is not.
        ("5h", FIVE_HOURS / 2, "{} 2:30"),
        ("7d", WEEK / 2, "{} 84h"),
        ("7d_oi", WEEK / 2, "Fable {}"),
    ],
)
@pytest.mark.parametrize(("used", "ahead"), [(0.6, True), (0.4, False)])
def test_each_window_is_bold_when_usage_runs_ahead_of_it(
    statusline: Statusline,
    name: str,
    left: float,
    text: str,
    used: float,
    *,
    ahead: bool,
) -> None:
    pct = f"{100 - round(used * 100)}%"
    shown = f"{BOLD}{pct}{NOBOLD}" if ahead else pct
    line = pooled(statusline, account("a", bucket(name, used, left)))
    assert line == f" {LEFT} · a {text.format(shown)}"


@pytest.mark.parametrize(
    ("name", "left", "text"),
    [
        ("5h", FIVE_HOURS / 2, "{} 2:30"),
        ("7d", WEEK / 2, "{} 84h"),
        ("7d_oi", WEEK / 2, "Fable {}"),
    ],
)
def test_run_rate_is_exact_at_the_boundary(
    statusline: Statusline, name: str, left: float, text: str
) -> None:
    # 50% used halfway through a window is not ahead; 51% is.
    line = pooled(
        statusline,
        account("a", bucket(name, 0.5, left)),
        account("b", bucket(name, 0.51, left)),
    )
    on_pace = text.format("50%")
    ahead = text.format(f"{BOLD}49%{NOBOLD}")
    assert line == f" {LEFT} · a {on_pace} · b {ahead}"


def test_unusable_pool(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(
        {"state": "unusable", "now": POOL_NOW, "current": None, "accounts": []}
    )
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · pool unusable"
    assert statusline.calls() == []


@pytest.mark.parametrize(
    "answer",
    [
        {},
        [],
        "usable",
        {**pool_answer(account("a")), "state": "other"},
        {k: v for k, v in pool_answer(account("a")).items() if k != "now"},
        {**pool_answer(account("a")), "now": "soon"},
        {**pool_answer(account("a")), "current": 5},
        {**pool_answer(account("a")), "accounts": {"x": account("a")}},
        {"state": "usable", "now": POOL_NOW, "current": None, "accounts": [5]},
        pool_answer({**account("a"), "held": "yes"}),
        pool_answer({**account("a"), "heldUntil": "later"}),
        pool_answer({**account("a"), "heldUntil": 1e300}),
        pool_answer({**account("a"), "heldUntil": -1}),
        pool_answer({**account("a", held=HOUR), "hold": "tired"}),
        pool_answer({**account("a"), "hold": 401}),
        pool_answer(),
        pool_answer(account("a"), now=-1),
        pool_answer(account("a"), now=1e300),
        pool_answer(account("a", {"bucket": "5h", "utilization": 0, "expires": 1e300})),
        pool_answer({**account("a"), "buckets": {}}),
        pool_answer(account("a", {"bucket": 5, "utilization": 0, "expires": 1})),
        pool_answer(account("a", {"bucket": "5h", "utilization": "0", "expires": 1})),
        pool_answer(account("a", {"bucket": "5h", "utilization": 0, "expires": "1"})),
        pool_answer(account("a"), {**account("b"), "name": 7}),
    ],
)
def test_a_cached_answer_that_is_not_the_object_is_a_failure(
    statusline: Statusline, answer: object
) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(answer)
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · {OWN_TEXT} · pool ??"


@pytest.mark.parametrize(
    ("answer", "text"),
    [
        (pool_answer(account("a"), now=0), "a ??"),
        (pool_answer(account("a"), now=4e12), "a ??"),
        (pool_answer(account("a"), now=4e12 + 1), "pool ??"),
        (
            pool_answer(
                account("a", {"bucket": "7d", "utilization": 0, "expires": 4e12})
            ),
            "a 100% 168h",
        ),
        (
            pool_answer(
                account("a", {"bucket": "7d", "utilization": 0, "expires": 4e12 + 1})
            ),
            "pool ??",
        ),
    ],
)
def test_times_are_valid_up_to_four_trillion_milliseconds(
    statusline: Statusline, answer: object, text: str
) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(answer)
    assert statusline.line() == f" {LEFT} · {text}"


@pytest.mark.parametrize(
    "name",
    [
        "",
        "-a",
        "A",
        "a b",
        "a_b",
        "a.b",
        "a\x1b[31mb",
        "a\nb",
        "x\nabc",
        "abc\n",
        "a·b",
        "é",
        "a" * 33,
        "a;b",
        "aB",
        "a-B",
        "abC-d",
    ],
)
def test_an_unsafe_account_name_fails_the_whole_answer(
    statusline: Statusline, name: str
) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(pool_answer(account("fine"), account(name)))
    result = statusline.line(OWN_SEGMENTS)
    assert result == f" {LEFT} · {OWN_TEXT} · pool ??"


@pytest.mark.parametrize("name", ["a", "0", "a-b-c", "a" * 32, "9a-"])
def test_names_in_the_accepted_pattern_are_shown(
    statusline: Statusline, name: str
) -> None:
    short = name.split("-", maxsplit=1)[0]
    assert pooled(statusline, account(name)) == f" {LEFT} · {short} ??"


def test_pooled_session_without_rate_limits_still_shows_the_pool(
    statusline: Statusline,
) -> None:
    line = pooled(statusline, account("a", bucket("7d", 0, WEEK)))
    assert line == f" {LEFT} · a 100% 168h"


@pytest.mark.parametrize(("used", "cost"), [(100, " · $0.50"), (99, "")])
def test_cost_keeps_its_rule_in_a_pooled_session(
    statusline: Statusline, used: int, cost: str
) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(pool_answer(account("a")))
    payload = {
        "cost": {"total_cost_usd": 0.5},
        "rate_limits": {"seven_day": window(used, 10 * HOUR)},
    }
    assert statusline.line(payload) == f" {LEFT} · a ??{cost}"


def test_first_run_fetches_the_pool_on_a_mac(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a", bucket("7d", 0, WEEK))))
    shutil.rmtree(statusline.usage.parent)
    assert statusline.line() == f" {LEFT} · a 100% 168h"
    assert statusline.calls() == ["curl"]
    assert statusline.urls() == [POOL_URL]
    ca = statusline.home / ".local" / "state" / "cog" / "ca.pem"
    argv = (statusline.log / "curl.argv").read_text().split()
    assert argv[:5] == ["-fsS", "--connect-timeout", "1", "--max-time", "2"]
    assert argv[argv.index("--cacert") + 1] == str(ca)
    assert json.loads(statusline.pool.read_text())["state"] == "usable"
    assert not list(statusline.pool.parent.glob("pool.json.*")), "temp file left"
    assert (statusline.log / "curl.proxy").read_text() == PROXY


def test_first_run_fetches_the_pool_on_linux_without_a_ca_file(
    statusline: Statusline,
) -> None:
    statusline.linux_pooled()
    statusline.pool_curl("ok", pool_answer(account("a", bucket("7d", 0, WEEK))))
    assert statusline.line() == f" {LEFT} · a 100% 168h"
    assert statusline.urls() == [POOL_URL]
    argv = (statusline.log / "curl.argv").read_text()
    assert "--cacert" not in argv
    assert argv.split()[:5] == ["-fsS", "--connect-timeout", "1", "--max-time", "2"]
    assert (statusline.log / "curl.proxy").read_text() == "unset"


@pytest.mark.parametrize("failure", ["http-error", "timeout", "garbage", "other"])
def test_a_failed_pool_fetch_is_visible_and_cached(
    statusline: Statusline, failure: str
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl(failure)
    # The usage endpoint would answer Fable, and must not be asked.
    statusline.curl("ok", FABLE_USAGE)
    statusline.credentials(CREDENTIALS)
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · {OWN_TEXT} · pool ??"
    assert json.loads(statusline.pool.read_text()) == {}
    assert not list(statusline.pool.parent.glob("pool.json.*")), "temp file left"
    # The failure lasts as long as a success would.
    statusline.pool_curl("ok", pool_answer(account("a")))
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · {OWN_TEXT} · pool ??"
    assert statusline.urls() == [POOL_URL]


def test_failed_pool_fetch_without_rate_limits_shows_only_the_failure(
    statusline: Statusline,
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("timeout")
    assert statusline.line() == f" {LEFT} · pool ??"


@pytest.mark.parametrize(
    ("age", "refetched"), [(30, True), (29, False), (600, True), (0, False)]
)
def test_pool_cache_lasts_thirty_seconds(
    statusline: Statusline, age: int, *, refetched: bool
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("fresh")))
    statusline.seed_pool(pool_answer(account("cached")), age=age)
    expected = "fresh" if refetched else "cached"
    assert statusline.line() == f" {LEFT} · {expected} ??"
    assert statusline.urls() == ([POOL_URL] if refetched else [])


def test_pool_is_fetched_once_inside_its_lifetime(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    first = statusline.line()
    statusline.pool_curl("timeout")
    assert statusline.line() == first
    assert statusline.urls() == [POOL_URL]


@pytest.mark.parametrize(("command", "fetched"), [("mktemp", False), ("mv", True)])
def test_an_answer_that_cannot_be_saved_does_not_show_the_old_one(
    statusline: Statusline, command: str, *, fetched: bool
) -> None:
    statusline.mac_pooled()
    statusline.seed_pool(pool_answer(account("old")), age=31)
    statusline.pool_curl("ok", pool_answer(account("new")))
    shim(statusline.home / "shims" / command, "exit 1\n")
    assert statusline.line() == f" {LEFT} · pool ??"
    assert statusline.urls() == ([POOL_URL] if fetched else [])
    assert not list(statusline.pool.parent.glob("pool.json.*")), "temp file left"


@pytest.mark.parametrize("ahead", [1, 2 * HOUR])
@pytest.mark.parametrize("cached", [pool_answer(account("cached")), {}])
def test_a_cache_dated_after_the_clock_is_refetched(
    statusline: Statusline, ahead: int, cached: object
) -> None:
    statusline.mac_pooled()
    statusline.env["STATUSLINE_TEST_REAL_MTIME"] = "1"
    statusline.pool_curl("ok", pool_answer(account("fresh")))
    statusline.seed_pool(cached, age=-ahead)
    assert statusline.line() == f" {LEFT} · fresh ??"
    assert statusline.urls() == [POOL_URL]


def test_a_missing_cache_root_is_created_for_the_pool(statusline: Statusline) -> None:
    root = statusline.home / "new" / "cache"
    statusline.env["XDG_CACHE_HOME"] = str(root)
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    assert statusline.line() == f" {LEFT} · a ??"
    assert (root / "claude-code-statusline" / "pool.json").exists()


def test_stale_temporary_files_are_swept_when_the_pool_is_fetched(
    statusline: Statusline,
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    stale, kept = seed_temporaries(statusline)
    assert statusline.line() == f" {LEFT} · a ??"
    assert [path for path in stale if path.exists()] == []
    assert all(path.exists() for path in kept)


ONE_ANSWER = json.dumps(pool_answer(account("a")))


@pytest.mark.parametrize("tail", ["", "\n", " \r\n\t\n"])
def test_whitespace_after_the_answer_is_accepted(
    statusline: Statusline, tail: str
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", raw=ONE_ANSWER + tail)
    assert statusline.line() == f" {LEFT} · a ??"


@pytest.mark.parametrize(
    "body",
    [
        f"{ONE_ANSWER}\n{ONE_ANSWER}",
        ONE_ANSWER + ONE_ANSWER,
        f"{{}}\n{ONE_ANSWER}",
        f"null\n{ONE_ANSWER}",
        f'{{"state": "unusable"}}\n{ONE_ANSWER}',
        f"{ONE_ANSWER}\n{{}}",
        f"{ONE_ANSWER}\nnot json",
        "",
        " \n",
        "null",
        f"[{ONE_ANSWER}]",
    ],
)
def test_a_body_that_is_not_one_answer_is_a_failure(
    statusline: Statusline, body: str
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", raw=body)
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · {OWN_TEXT} · pool ??"
    assert json.loads(statusline.pool.read_text()) == {}


def test_a_curl_error_message_stays_off_the_status_line(
    statusline: Statusline,
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("noisy-http-error")
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · {OWN_TEXT} · pool ??"


def test_an_mv_error_message_stays_off_the_status_line(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    shim(statusline.home / "shims" / "mv", "echo 'mv: cannot move' >&2\nexit 1\n")
    assert statusline.line() == f" {LEFT} · pool ??"


def test_a_cache_that_is_not_json_stays_off_the_status_line(
    statusline: Statusline,
) -> None:
    statusline.mac_pooled()
    _ = statusline.pool.write_text("not json")
    os.utime(statusline.pool, (NOW, NOW))
    assert statusline.line(OWN_SEGMENTS) == f" {LEFT} · {OWN_TEXT} · pool ??"


def test_the_pool_answer_has_a_size_limit(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    _ = statusline.line()
    argv = (statusline.log / "curl.argv").read_text().split()
    assert argv[argv.index("--max-filesize") + 1] == "1048576"


@pytest.mark.parametrize("mac", [True, False])
def test_the_pool_fetch_verifies_the_certificate(
    statusline: Statusline, *, mac: bool
) -> None:
    if mac:
        statusline.mac_pooled()
    else:
        statusline.linux_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    _ = statusline.line()
    flags = [
        arg
        for arg in (statusline.log / "curl.argv").read_text().split()
        if arg[0] == "-"
    ]
    assert not {"--insecure", "--proxy-insecure"} & set(flags)
    assert not [flag for flag in flags if flag[1] != "-" and "k" in flag]


def test_unwritable_cache_directory_means_no_pool_fetch(
    statusline: Statusline,
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a")))
    shutil.rmtree(statusline.usage.parent)
    _ = statusline.usage.parent.write_text("")
    assert statusline.line() == f" {LEFT} · pool ??"
    assert statusline.urls() == []


@pytest.mark.parametrize(
    "ostype",
    ["darwin25.0", "linux-gnu"],
)
def test_a_pooled_session_never_asks_for_usage_or_reads_the_login(
    statusline: Statusline, ostype: str
) -> None:
    if ostype.startswith("darwin"):
        statusline.mac_pooled()
    else:
        statusline.linux_pooled()
    statusline.env["CLAUDE_CODE_OAUTH_TOKEN"] = "cog"  # noqa: S105
    statusline.env["STATUSLINE_TEST_KEYCHAIN"] = json.dumps(CREDENTIALS)
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.pool_curl("ok", pool_answer(account("a")))
    _ = statusline.line(SUBSCRIBER)
    statusline.seed_pool({}, age=100)
    _ = statusline.line(SUBSCRIBER)
    assert statusline.calls() == ["curl", "curl"]
    assert statusline.urls() == [POOL_URL, POOL_URL]
    assert not statusline.usage.exists()
    assert "Authorization" not in (statusline.log / "curl.headers").read_text()
    assert TOKEN not in (statusline.log / "curl.argv").read_text()


def test_the_proxy_key_reaches_no_argument_file_or_output(
    statusline: Statusline,
) -> None:
    statusline.mac_pooled()
    statusline.pool_curl("ok", pool_answer(account("a", bucket("7d", 0, WEEK))))
    result = statusline.run(OWN_SEGMENTS)
    assert result.returncode == 0
    assert PROXY_KEY not in result.stdout + result.stderr
    for name in ("curl.argv", "curl.headers", "calls", "urls"):
        assert PROXY_KEY not in (statusline.log / name).read_text()
    for path in statusline.pool.parent.rglob("*"):
        if path.is_file():
            assert PROXY_KEY not in path.read_text()
    assert PROXY_KEY in (statusline.log / "curl.proxy").read_text()


def test_the_proxy_key_is_not_printed_by_a_trace(statusline: Statusline) -> None:
    statusline.mac_pooled()
    statusline.env["SHELLOPTS"] = "xtrace"
    statusline.seed_pool(pool_answer(account("a")))
    result = statusline.run()
    assert result.returncode == 0
    assert PROXY_KEY not in result.stdout + result.stderr


@pytest.mark.parametrize(
    "proxy",
    [
        PROXY,
        "http://cog:k@[::1]:1",
        "http://cog:A-z_0@[::1]:65535",
    ],
)
def test_the_proxy_pattern_matches_cogs_url_with_any_port(
    statusline: Statusline, proxy: str
) -> None:
    statusline.mac_pooled(proxy)
    statusline.seed_pool(pool_answer(account("a")))
    assert statusline.line() == f" {LEFT} · a ??"


@pytest.mark.parametrize(
    ("lower", "pooled_session"), [("", True), ("http://corp.example:3128", False)]
)
def test_the_lower_case_proxy_that_curl_prefers_decides_the_session(
    statusline: Statusline, lower: str, *, pooled_session: bool
) -> None:
    statusline.mac_pooled()
    statusline.env["https_proxy"] = lower
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.seed_pool(pool_answer(account("a")))
    expected = (
        f" {LEFT} · a ??"
        if pooled_session
        else (f" {LEFT} · 100h 60% · {FABLE_SEGMENT}")
    )
    assert statusline.line(SUBSCRIBER) == expected


@pytest.mark.parametrize(
    "proxy",
    [
        "http://corp.example:3128",
        "http://cog:key@127.0.0.1:17443",
        "http://cog:key@localhost:17443",
        "http://user:key@[::1]:17443",
        "https://cog:key@[::1]:17443",
        "http://[::1]:17443",
        "http://cog@[::1]:17443",
        "http://cog:k@[::1]:",
        "http://cog:k@[::1]",
        "http://cog:@[::1]:17443",
        "http://cog:k@[::1]:17443/",
        "http://cog:k@[::1]:17443\n",
        "http://cog:k@evil.example:80/@[::1]:1",
        "http://cog:k k@[::1]:17443",
        "xhttp://cog:k@[::1]:1",
        "http://corp.example:3128/?u=http://cog:k@[::1]:1",
        "",
    ],
)
def test_another_proxy_on_a_mac_is_not_a_pooled_session(
    statusline: Statusline, proxy: str
) -> None:
    statusline.mac_pooled(proxy)
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.pool_curl("ok", pool_answer(account("a")))
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.urls() == [USAGE_URL]


def test_a_guests_marker_and_placeholder_do_not_make_a_mac_session_pooled(
    statusline: Statusline,
) -> None:
    statusline.linux_pooled()
    statusline.env["OSTYPE"] = "darwin25.0"
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.urls() == [USAGE_URL]


def test_linux_session_with_marker_and_placeholder_is_pooled(
    statusline: Statusline,
) -> None:
    statusline.linux_pooled()
    statusline.seed_pool(pool_answer(account("a")))
    assert statusline.line() == f" {LEFT} · a ??"


@pytest.mark.parametrize("machine", ["mac", "linux"])
@pytest.mark.parametrize(
    "base",
    [
        "https://api.fireworks.ai/inference",
        "https://api.anthropic.com.example",
        "http://api.anthropic.com",
        "https://example.com/https://api.anthropic.com/",
    ],
)
def test_a_session_on_another_provider_is_not_pooled(
    statusline: Statusline, machine: str, base: str
) -> None:
    if machine == "mac":
        statusline.mac_pooled()
    else:
        statusline.linux_pooled()
    statusline.env["ANTHROPIC_BASE_URL"] = base
    statusline.pool_curl("ok", pool_answer(account("a")))
    assert statusline.line({"model": {"display_name": "GLM-5.3"}}) == (
        f" {LEFT} · GLM-5.3"
    )
    assert statusline.urls() == []


@pytest.mark.parametrize("machine", ["mac", "linux"])
@pytest.mark.parametrize(
    "base", ["https://api.anthropic.com", "https://api.anthropic.com/", ""]
)
def test_a_base_url_that_names_anthropic_keeps_the_session_pooled(
    statusline: Statusline, machine: str, base: str
) -> None:
    if machine == "mac":
        statusline.mac_pooled()
    else:
        statusline.linux_pooled()
    statusline.env["ANTHROPIC_BASE_URL"] = base
    statusline.seed_pool(pool_answer(account("a")))
    assert statusline.line() == f" {LEFT} · a ??"


@pytest.mark.parametrize("token", ["cog", "cog\n", "cog\n\n"])
def test_the_placeholder_is_read_as_the_zshrc_reads_it(
    statusline: Statusline, token: str
) -> None:
    statusline.linux_pooled(token)
    statusline.seed_pool(pool_answer(account("a")))
    assert statusline.line() == f" {LEFT} · a ??"


@pytest.mark.parametrize(
    "token",
    [
        "",
        "cog2",
        "xcog",
        "sk-ant-oat-other",
        "cog cog",
        " cog",
        "cog ",
        "co g",
        "cog\t",
        "\ncog",
        "cog\r",
    ],
)
def test_marker_with_another_token_is_not_pooled_on_linux(
    statusline: Statusline, token: str
) -> None:
    statusline.linux_pooled(token)
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.pool_curl("ok", pool_answer(account("a")))
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.urls() == [USAGE_URL]


def test_placeholder_without_the_marker_is_not_pooled_on_linux(
    statusline: Statusline,
) -> None:
    statusline.linux_pooled()
    (statusline.home / "cog-gateway").unlink()
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    statusline.pool_curl("ok", pool_answer(account("a")))
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.urls() == [USAGE_URL]


def test_marker_without_a_token_file_is_not_pooled_on_linux(
    statusline: Statusline,
) -> None:
    statusline.linux_pooled()
    (statusline.home / ".config" / "secrets" / "CLAUDE_CODE_OAUTH_TOKEN").unlink()
    assert statusline.render(SUBSCRIBER) == f" {LEFT} · 100h 60% · Fable ??"


def test_a_cog_proxy_on_linux_is_not_a_pooled_session(
    statusline: Statusline,
) -> None:
    statusline.env["OSTYPE"] = "linux-gnu"
    statusline.env["HTTPS_PROXY"] = PROXY
    statusline.env["DOTFILES_GATEWAY_MARKER"] = str(statusline.home / "absent")
    statusline.credentials(CREDENTIALS)
    statusline.curl("ok", FABLE_USAGE)
    assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · {FABLE_SEGMENT}"
    assert statusline.urls() == [USAGE_URL]
    assert (statusline.log / "curl.proxy").read_text() == PROXY
