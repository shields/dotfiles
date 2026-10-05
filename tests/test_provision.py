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

# provision.sh changes the machine it runs on, so it is never executed here.
# Its structure is checked from the text.

import re
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
PROVISION = REPO / "provision.sh"
MACOS = REPO / "provision" / "macos.sh"
SCRIPTS = (
    PROVISION,
    MACOS,
    REPO / "provision" / "modules.sh",
    REPO / "provision" / "linux-system.sh",
)
SCRIPT_IDS = [script.name for script in SCRIPTS]
TEXT = PROVISION.read_text()
LINES = TEXT.splitlines()

MACOS_GUARD = "if [[ $os == macos ]]; then"
MACOS_FUNCTIONS = (
    "macos_preflight",
    "macos_xcode",
    "macos_login_shell",
    "macos_defaults",
    "macos_emacs_app",
    "macos_finish",
)

# The oldest bash provision.sh must run under is macOS's 3.2.
BASH_4_FEATURES = {
    "mapfile": r"\bmapfile\b",
    "readarray": r"\breadarray\b",
    "associative arrays": r"\b(declare|local|typeset)\s+-[a-zA-Z]*A\b",
    "case modification": r"\$\{[^}]*(,,|\^\^)[^}]*\}",
    "[[ -v": r"\[\[\s+-v\b",
    "&>>": r"&>>",
    "|&": r"\|&",
    "case fallthrough": r";;&|;&",
    "coproc": r"\bcoproc\b",
    "shopt features": r"shopt\s+-s\s+(globstar|lastpipe|inherit_errexit)",
    "wait -n": r"\bwait\s+-n\b",
}


def indent_of(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def openers(index: int) -> list[str]:
    """The block-opening lines that enclose LINES[index], innermost first."""
    indent = indent_of(LINES[index])
    found: list[str] = []
    for line in reversed(LINES[:index]):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if indent_of(line) < indent:
            found.append(stripped)
            indent = indent_of(line)
            if indent == 0:
                break
    return found


def find(pattern: str, start: int = 0) -> int:
    for i in range(start, len(LINES)):
        if re.search(pattern, LINES[i]):
            return i
    msg = f"no line matches {pattern!r} from line {start + 1}"
    raise AssertionError(msg)


@pytest.mark.parametrize("script", SCRIPTS, ids=SCRIPT_IDS)
def test_scripts_parse_under_the_oldest_bash(script: Path) -> None:
    result = subprocess.run(
        ["/bin/bash", "-n", str(script)],
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )
    assert result.returncode == 0, result.stderr


@pytest.mark.parametrize("script", SCRIPTS, ids=SCRIPT_IDS)
def test_scripts_avoid_bash_4_features(script: Path) -> None:
    text = "\n".join(
        line for line in script.read_text().splitlines() if not line.startswith("#")
    )
    found = [
        name for name, pattern in BASH_4_FEATURES.items() if re.search(pattern, text)
    ]
    assert found == []


def test_os_is_detected_once_and_others_are_refused() -> None:
    assert len(re.findall(r"\buname -s\b", TEXT)) == 1
    detect = find(r"^uname_s=\$\(uname -s\)")
    assert LINES[detect + 2] == "Darwin) os=macos ;;"
    assert "unsupported OS $uname_s" in TEXT


def test_macos_functions_are_defined_in_macos_sh() -> None:
    lines = MACOS.read_text().splitlines()
    assert lines[0] == "# shellcheck shell=bash"
    defined = [line[:-4] for line in lines if line.endswith("() {")]
    assert defined == list(MACOS_FUNCTIONS)
    toplevel = [
        line
        for line in lines
        if line and not line.startswith(("#", " ", "}")) and not line.endswith("() {")
    ]
    assert toplevel == []


def test_macos_functions_are_called_as_plain_statements_in_the_guard() -> None:
    for name in MACOS_FUNCTIONS:
        uses = [i for i, line in enumerate(LINES) if re.search(rf"\b{name}\b", line)]
        assert len(uses) == 1, name
        assert LINES[uses[0]].strip() == name
        assert openers(uses[0])[:1] == [MACOS_GUARD]


def test_macos_functions_are_called_in_the_original_order() -> None:
    calls = [find(rf"^\s+{name}$") for name in MACOS_FUNCTIONS]
    assert calls == sorted(calls)
    preflight, xcode, login_shell, defaults, emacs_app, finish = calls
    analytics = find(r"^brew analytics off")
    oh_my_zsh = find(r"Oh My Zsh installation")
    assert analytics < preflight < oh_my_zsh
    assert find(r"^bin/docker-prune") < xcode
    assert login_shell == xcode + 1
    assert find(r"gcloud --quiet components update") < defaults
    emacs = find(r"^emacs --batch --script \.emacs\.d/provision\.el")
    assert defaults < emacs < emacs_app
    assert emacs_app < find(r"Bootstrap TLS trust")
    assert find(r"configure_codex\.py") < finish
    assert [line.strip() for line in LINES[finish + 1 :] if line.strip()] == ["fi"]


def test_macos_sh_is_only_sourced_on_macos() -> None:
    sourced = find(r"^\s+source provision/macos\.sh$")
    assert openers(sourced)[:1] == [MACOS_GUARD]
