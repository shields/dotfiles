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
# and curl and the keychain shimmed so the usage fetch can be driven through
# its success and failure paths without reaching the network.

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

# Records the call, the argv and the headers (including any read from an
# @file, as curl would), then answers as STATUSLINE_TEST_CURL directs: a
# body written to the -o file, or one of curl's failure exit codes.
CURL_SHIM = r"""
log="$STATUSLINE_TEST_LOG"
printf '%s\n' curl >> "$log/calls"
printf '%s\n' "$*" > "$log/curl.argv"
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
case "${STATUSLINE_TEST_CURL:-}" in
    ok) printf '%s' "$STATUSLINE_TEST_RESPONSE" > "$out" ;;
    garbage) printf 'not json' > "$out" ;;
    http-error) exit 22 ;;
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


def shim(target: Path, body: str) -> None:
    _ = target.write_text(f"#!/bin/bash\n{body}")
    target.chmod(0o755)


@final
class Statusline:
    def __init__(self, home: Path) -> None:
        self.home = home
        self.usage = home / "cache" / "claude-code-statusline" / "usage.json"
        self.usage.parent.mkdir(parents=True)
        self.log = home / "log"
        self.log.mkdir()
        shims = home / "shims"
        shims.mkdir()
        self.env = dict(os.environ)
        self.env.update(
            PATH=f"{shims}:{os.environ['PATH']}",
            HOME=str(home),
            XDG_CACHE_HOME=str(home / "cache"),
            STATUSLINE_TEST_LOG=str(self.log),
        )
        shim(shims / "starship", STARSHIP_SHIM)
        shim(shims / "curl", CURL_SHIM)
        shim(shims / "security", SECURITY_SHIM)
        shim(
            shims / "date",
            rf"""
if [[ $# -eq 1 && "$1" == "+%s" ]]; then
    printf '%s\n' {NOW}
else
    exec /bin/date "$@"
fi
""",
        )

    def cache(self, usage: object, *, age: float = 0) -> None:
        """Seed the usage cache, `age` seconds old by the script's clock."""
        _ = self.usage.write_text(json.dumps(usage))
        os.utime(self.usage, (NOW - age, NOW - age))

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


def test_no_token_means_no_request(statusline: Statusline) -> None:
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
    statusline.usage.parent.chmod(0o500)
    try:
        assert statusline.line(SUBSCRIBER) == f" {LEFT} · 100h 60% · Fable ??"
    finally:
        statusline.usage.parent.chmod(0o700)
    assert statusline.calls() == []
    assert not statusline.usage.exists()
