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
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

CONFTEST = Path(__file__).with_name("conftest.py")

SKIPS = """
import pytest


def test_runtime_skip():
    pytest.skip("tool is missing")


@pytest.mark.skipif(True, reason="marked")
def test_marked_skip():
    pass


@pytest.mark.xfail(reason="expected")
def test_expected_failure():
    raise AssertionError
"""

MODULE_SKIP = """
import pytest

pytest.skip("no tool", allow_module_level=True)
"""

PASSES = """
def test_ok():
    pass
"""


def run_pytest(
    tmp_path: Path, files: dict[str, str], *, fail_on_skip: str | None
) -> subprocess.CompletedProcess[str]:
    _ = shutil.copy(CONFTEST, tmp_path / "conftest.py")
    for name, content in files.items():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(content)
    env = {
        k: v
        for k, v in os.environ.items()
        if not k.startswith("DOTFILES_") and k != "PYTEST_ADDOPTS"
    }
    if fail_on_skip is not None:
        env["DOTFILES_FAIL_ON_SKIP"] = fail_on_skip
    return subprocess.run(
        [sys.executable, "-m", "pytest", "-p", "no:cacheprovider", "-q"],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )


@pytest.mark.parametrize("tests", [SKIPS, MODULE_SKIP])
def test_a_skip_fails_the_run_when_asked(tmp_path: Path, tests: str) -> None:
    result = run_pytest(tmp_path, {"test_sample.py": tests}, fail_on_skip="1")
    assert result.returncode == 1, result.stdout
    assert "DOTFILES_FAIL_ON_SKIP: skipped tests" in result.stdout
    assert "test_sample.py" in result.stdout


def test_a_skip_in_a_nested_conftest_fails_the_run(tmp_path: Path) -> None:
    files = {
        "sub/conftest.py": MODULE_SKIP,
        "sub/test_inner.py": PASSES,
        "test_sample.py": PASSES,
    }
    result = run_pytest(tmp_path, files, fail_on_skip="1")
    assert result.returncode == 1, result.stdout
    assert "DOTFILES_FAIL_ON_SKIP: skipped tests" in result.stdout


def test_the_skipped_tests_are_named(tmp_path: Path) -> None:
    result = run_pytest(tmp_path, {"test_sample.py": SKIPS}, fail_on_skip="1")
    assert "test_sample.py::test_runtime_skip: " in result.stdout
    assert "tool is missing" in result.stdout
    assert "test_sample.py::test_marked_skip: " in result.stdout
    assert "marked" in result.stdout
    assert "test_expected_failure" not in result.stdout.split("skipped tests")[-1]


@pytest.mark.parametrize("fail_on_skip", [None, "", "0"])
@pytest.mark.parametrize("tests", [SKIPS, MODULE_SKIP])
def test_skips_are_allowed_unless_the_variable_is_one(
    tmp_path: Path, tests: str, fail_on_skip: str | None
) -> None:
    result = run_pytest(tmp_path, {"test_sample.py": tests}, fail_on_skip=fail_on_skip)
    assert result.returncode in {pytest.ExitCode.OK, pytest.ExitCode.NO_TESTS_COLLECTED}
    assert "DOTFILES_FAIL_ON_SKIP" not in result.stdout


def test_a_worse_exit_status_is_kept(tmp_path: Path) -> None:
    files = {"test_sample.py": MODULE_SKIP, "test_broken.py": "def test_broken(:\n"}
    result = run_pytest(tmp_path, files, fail_on_skip="1")
    assert result.returncode == pytest.ExitCode.INTERRUPTED, result.stdout


def test_a_run_without_skips_passes_when_asked(tmp_path: Path) -> None:
    result = run_pytest(tmp_path, {"test_sample.py": PASSES}, fail_on_skip="1")
    assert result.returncode == 0, result.stdout


@pytest.mark.parametrize("value", ["true", "yes", "01", " 1"])
def test_an_unrecognized_value_is_a_usage_error(tmp_path: Path, value: str) -> None:
    result = run_pytest(tmp_path, {"test_sample.py": SKIPS}, fail_on_skip=value)
    assert result.returncode == pytest.ExitCode.USAGE_ERROR, result.stdout
    assert "DOTFILES_FAIL_ON_SKIP must be 1 or 0" in result.stderr


def test_the_options_of_the_outer_run_do_not_leak(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    monkeypatch.setenv("PYTEST_ADDOPTS", "-ra --no-summary")
    result = run_pytest(tmp_path, {"test_sample.py": SKIPS}, fail_on_skip="1")
    assert "DOTFILES_FAIL_ON_SKIP: skipped tests" in result.stdout
    assert "test_expected_failure" not in result.stdout.split("skipped tests")[-1]
