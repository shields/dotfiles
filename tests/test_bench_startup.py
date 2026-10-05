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
import shlex
import shutil
import subprocess
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
BENCH = REPO / "tools/bench_startup.zsh"

HYPERFINE = """#!/bin/sh
for arg in "$@"; do
    case $arg in
    *bench-1*)
        set -- $arg
        cp "$5/.zshenv" "$CAPTURED"
        ;;
    esac
done
"""


def test_generated_zshenv_runs_the_repos_zshenv_first(tmp_path: Path) -> None:
    zsh = shutil.which("zsh")
    assert zsh, "zsh is required"
    stubs = tmp_path / "stubs"
    stubs.mkdir()
    (tmp_path / "home").mkdir()
    captured = tmp_path / "generated-zshenv"
    # The benchmark deletes its throwaway ZDOTDIRs when it exits, so the
    # stand-in for hyperfine copies out the .zshenv that the first candidate's
    # command line names: zsh -f DRIVER START_DIR ZDOTDIR zsh -l -i.
    hyperfine = stubs / "hyperfine"
    _ = hyperfine.write_text(HYPERFINE)
    hyperfine.chmod(0o755)
    env = dict(os.environ)
    env.update(
        CAPTURED=str(captured),
        HOME=str(tmp_path / "home"),
        PATH=f"{stubs}{os.pathsep}{env['PATH']}",
    )
    result = subprocess.run(
        [zsh, str(BENCH)],
        env=env,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    lines = captured.read_text().splitlines()
    assert shlex.split(lines[0]) == ["source", str(REPO / ".zshenv")]
    assert any(line.startswith("HISTFILE=") for line in lines[1:])
