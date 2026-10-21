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
from typing import TYPE_CHECKING

import pytest

if TYPE_CHECKING:
    from collections.abc import Iterator


FAIL_ON_SKIP = "DOTFILES_FAIL_ON_SKIP"


class SkipGate:
    """A plugin object, not module-level hooks in this conftest.py.

    pytest drops a conftest's hooks for a directory whose own conftest.py
    raised Skipped when it was imported, which would hide that skip.
    """

    def __init__(self) -> None:
        self.skipped: dict[str, str] = {}

    def record(self, report: pytest.CollectReport | pytest.TestReport) -> None:
        longrepr = report.longrepr
        reason = ""
        if isinstance(longrepr, tuple) and len(longrepr) == 3:
            reason = str(longrepr[2])
        self.skipped[report.nodeid] = reason

    def pytest_collectreport(self, report: pytest.CollectReport) -> None:
        if report.skipped:
            self.record(report)

    def pytest_runtest_logreport(self, report: pytest.TestReport) -> None:
        if report.skipped and not hasattr(report, "wasxfail"):
            self.record(report)

    def pytest_terminal_summary(
        self, terminalreporter: pytest.TerminalReporter
    ) -> None:
        if self.skipped:
            terminalreporter.section(f"{FAIL_ON_SKIP}: skipped tests", red=True)
            for nodeid, reason in self.skipped.items():
                terminalreporter.line(f"{nodeid}: {reason}")

    def pytest_sessionfinish(self, session: pytest.Session) -> None:
        passing = {pytest.ExitCode.OK, pytest.ExitCode.NO_TESTS_COLLECTED}
        if self.skipped and session.exitstatus in passing:
            session.exitstatus = pytest.ExitCode.TESTS_FAILED


def pytest_configure(config: pytest.Config) -> None:
    value = os.environ.get(FAIL_ON_SKIP, "")
    if value not in {"", "0", "1"}:
        msg = f"{FAIL_ON_SKIP} must be 1 or 0, not {value!r}"
        raise pytest.UsageError(msg)
    if value == "1":
        _ = config.pluginmanager.register(SkipGate(), "fail-on-skip")


@pytest.fixture(autouse=True)
def _fixed_umask() -> Iterator[None]:
    # Fixtures create files and directories with the caller's umask, and
    # compaudit treats a group-writable directory as insecure. Debian's
    # pam_umask gives ssh sessions 002.
    previous = os.umask(0o022)
    yield
    _ = os.umask(previous)
