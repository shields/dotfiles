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
# and the usage cache pre-seeded so nothing reaches the network or the
# keychain.

import json
import os
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
# The clock the script sees. It must stay close to real time so the usage
# cache written by each case counts as fresh and the script never fetches.
NOW = int(time.time())
# What the starship shim's colored, padded output must be reduced to.
LEFT = "main (abc1234) [!]"
FABLE_USAGE = {
    "limits": [{"scope": {"model": {"display_name": "Fable"}}, "percent": 45.7}]
}

STARSHIP_SHIM = r"""
[[ -n "${STATUSLINE_TEST_STARSHIP_FAIL:-}" ]] && exit 1
case "$2" in
    git_branch) printf ' \033[1;35m main\033[0m ' ;;
    git_commit) printf '\033[32m(abc1234)\033[0m ' ;;
    git_status) printf '\033[31m[!]\033[0m ' ;;
    *) exit 1 ;;
esac
"""


def window(used: object, left: float) -> dict[str, object]:
    """A rate-limit window `used` percent spent, resetting `left` seconds from now."""
    return {"used_percentage": used, "resets_at": NOW + left}


def shim(target: Path, body: str) -> None:
    _ = target.write_text(f"#!/bin/bash\n{body}")
    target.chmod(0o755)


@final
class Statusline:
    def __init__(self, home: Path) -> None:
        self.home = home
        self.calls = home / "calls"
        self.usage = home / "cache" / "claude-code-statusline" / "usage.json"
        self.usage.parent.mkdir(parents=True)
        shims = home / "shims"
        shims.mkdir()
        self.env = dict(os.environ)
        self.env.update(
            PATH=f"{shims}:{os.environ['PATH']}",
            HOME=str(home),
            XDG_CACHE_HOME=str(home / "cache"),
        )
        shim(shims / "starship", STARSHIP_SHIM)
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
        # The script must never get as far as fetching usage; record any
        # attempt so a case can fail on it rather than hang or leak.
        for name in ("curl", "security"):
            shim(shims / name, f"printf '%s\\n' {name} >> '{self.calls}'\nexit 1\n")

    def run(
        self, payload: object = None, *, usage: object = None
    ) -> subprocess.CompletedProcess[str]:
        _ = self.usage.write_text(json.dumps({} if usage is None else usage))
        result = subprocess.run(
            [str(SCRIPT)],
            input="" if payload is None else json.dumps(payload),
            capture_output=True,
            text=True,
            env=self.env,
            cwd=self.home,
            check=False,
        )
        assert not self.calls.exists(), "the script tried to fetch usage"
        return result

    def render(self, payload: object = None, *, usage: object = None) -> str:
        result = self.run(payload, usage=usage)
        assert result.returncode == 0, result.stderr
        assert result.stderr == ""
        return result.stdout


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
    # 45.7% used, floored, at 40% of the week elapsed is ahead of pace.
    payload = {"rate_limits": {"seven_day": window(40, 100 * HOUR)}}
    line = statusline.render(payload, usage=FABLE_USAGE)
    assert line == f" {LEFT} · 100h 60% · Fable {BOLD}55%{NOBOLD}"


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
    payload = {"rate_limits": {"seven_day": window(40, 100 * HOUR)}}
    assert statusline.render(payload, usage=usage) == f" {LEFT} · 100h 60% · Fable ??"


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
