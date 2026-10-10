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

import importlib.util
import json
import os
import shutil
import subprocess
import sys
import tomllib
from pathlib import Path
from typing import TYPE_CHECKING, Protocol, cast, final

import pytest

if TYPE_CHECKING:
    from collections.abc import Mapping

type Json = dict[str, object]

REPO = Path(__file__).resolve().parents[1]
TOOL = REPO / "tools/configure_codex.py"
HOME = Path("/home/me")
PROJECTS = "/home/me/src/github.com/shields/dotfiles"
DEVELOPMENT_PERMISSIONS: Json = {
    "extends": ":workspace",
    "filesystem": {"~/.cache/uv": "write"},
    "network": {
        "enabled": True,
        "allow_local_binding": True,
        "domains": {"localhost": "allow", "127.0.0.1": "allow"},
    },
}

STUB = """\
#!PYTHON
import json
import os
import sys
from pathlib import Path

state = Path(os.environ["STUB_DIR"])
launch = {"argv": sys.argv[1:], "codex_home": os.environ.get("CODEX_HOME")}
state.joinpath("launch.json").write_text(json.dumps(launch))
script = json.loads(state.joinpath("script.json").read_text())
for line in sys.stdin:
    with state.joinpath("received.jsonl").open("a") as log:
        log.write(line)
    message = json.loads(line)
    if "id" not in message:
        continue
    if message["method"] == script.get("exit_on"):
        sys.exit(0)
    for unsolicited in script.get("before", []):
        print(json.dumps(unsolicited), flush=True)
    reply = script["replies"].get(message["method"], {"result": {}})
    print(json.dumps({"id": message["id"], **reply}), flush=True)
"""


class Tool(Protocol):
    LGTMCP_POLICY: str

    def settings(self, config: Mapping[str, object], home: Path) -> Json: ...

    def hook_state(self, listing: Mapping[str, object], hooks_file: Path) -> Json: ...

    def configure_codex(self, config_file: Path, home: Path) -> None: ...


def load_tool() -> Tool:
    spec = importlib.util.spec_from_file_location("configure_codex", TOOL)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return cast("Tool", cast("object", module))


tool = load_tool()


def hook(source: Path, key: str, current_hash: str) -> Json:
    return {
        "key": key,
        "eventName": "preToolUse",
        "sourcePath": str(source),
        "source": "user",
        "currentHash": current_hash,
        "trustStatus": "untrusted",
    }


def listing(
    hooks: list[Json],
    errors: list[dict[str, str]] | None = None,
    warnings: list[str] | None = None,
) -> Json:
    return {
        "data": [
            {
                "cwd": "/x",
                "hooks": hooks,
                "warnings": warnings or [],
                "errors": errors or [],
            }
        ]
    }


def test_settings_for_an_empty_config() -> None:
    assert tool.settings({}, HOME) == {
        "auto_review.extra_policy": tool.LGTMCP_POLICY,
        "mcp_servers.lgtmcp.tools.review_only.approval_mode": "approve",
        "mcp_servers.lgtmcp.tools.review_and_commit.approval_mode": "approve",
        "approvals_reviewer": "auto_review",
        "features.worktrees": True,
        "features.network_proxy": True,
        "default_permissions": "dev",
        "permissions.dev": DEVELOPMENT_PERMISSIONS,
        "projects": {PROJECTS: {"trust_level": "trusted"}},
    }


def test_settings_append_to_an_existing_policy() -> None:
    config: Json = {"auto_review": {"extra_policy": "Keep this."}}
    policy = tool.settings(config, HOME)["auto_review.extra_policy"]
    assert policy == f"Keep this.\n\n{tool.LGTMCP_POLICY}"


def test_settings_do_not_repeat_the_policy() -> None:
    policy_text = f"A\n\n{tool.LGTMCP_POLICY}\n\nB"
    config: Json = {"auto_review": {"extra_policy": policy_text}}
    policy = tool.settings(config, HOME)["auto_review.extra_policy"]
    assert policy == policy_text


@pytest.mark.parametrize("trust_level", ["trusted", "untrusted"])
def test_settings_keep_an_existing_project_entry(trust_level: str) -> None:
    config: Json = {"projects": {PROJECTS: {"trust_level": trust_level}}}
    assert "projects" not in tool.settings(config, HOME)


def test_settings_follow_the_home_directory() -> None:
    projects = tool.settings({}, Path("/Users/someone"))["projects"]
    assert list(cast("Json", projects)) == [
        "/Users/someone/src/github.com/shields/dotfiles"
    ]


def test_settings_keep_existing_permission_rules() -> None:
    development: Json = {
        "description": "Custom development rules",
        "workspace_roots": {"~/src/shared": True},
        "filesystem": {"~/.ssh": "deny"},
        "network": {
            "allow_upstream_proxy": False,
            "domains": {"example.com": "deny"},
            "unix_sockets": {"/var/run/docker.sock": "deny"},
        },
    }
    original = json.dumps(development)
    result = cast(
        "Json",
        tool.settings({"permissions": {"dev": development}}, HOME)["permissions.dev"],
    )
    assert result["description"] == development["description"]
    assert result["workspace_roots"] == development["workspace_roots"]
    assert result["filesystem"] == {"~/.ssh": "deny", "~/.cache/uv": "write"}
    assert result["network"] == {
        "allow_upstream_proxy": False,
        "enabled": True,
        "allow_local_binding": True,
        "domains": {"example.com": "deny", "localhost": "allow", "127.0.0.1": "allow"},
        "unix_sockets": {"/var/run/docker.sock": "deny"},
    }
    assert json.dumps(development) == original


@pytest.mark.parametrize(
    "config",
    [
        {"auto_review": "text"},
        {"auto_review": {"extra_policy": ["a"]}},
        {"projects": "text"},
        {"permissions": "text"},
        {"permissions": {"dev": "text"}},
        {"permissions": {"dev": {"filesystem": "text"}}},
        {"permissions": {"dev": {"network": "text"}}},
        {"permissions": {"dev": {"network": {"domains": "text"}}}},
    ],
)
def test_settings_reject_malformed_config(config: Json) -> None:
    with pytest.raises(TypeError):
        _ = tool.settings(config, HOME)


def test_hook_state_trusts_only_the_installed_hooks_file(tmp_path: Path) -> None:
    installed = tmp_path.resolve() / ".codex/hooks.json"
    project = tmp_path.resolve() / "repo/.codex/hooks.json"
    state = tool.hook_state(
        listing(
            [
                hook(installed, f"{installed}:pre_tool_use:0:0", "sha256:aa"),
                hook(installed, f"{installed}:stop:0:0", "sha256:bb"),
                hook(project, f"{project}:pre_tool_use:0:0", "sha256:cc"),
            ]
        ),
        installed,
    )
    assert state == {
        f"{installed}:pre_tool_use:0:0": {"trusted_hash": "sha256:aa"},
        f"{installed}:stop:0:0": {"trusted_hash": "sha256:bb"},
    }


def test_hook_state_follows_symlinked_source_paths(tmp_path: Path) -> None:
    real = tmp_path.resolve() / "real"
    real.mkdir()
    link = tmp_path / "link"
    link.symlink_to(real)
    installed = real / "hooks.json"
    state = tool.hook_state(
        listing([hook(link / "hooks.json", "key", "sha256:aa")]), installed
    )
    assert state == {"key": {"trusted_hash": "sha256:aa"}}


def test_hook_state_follows_a_symlinked_hooks_file(tmp_path: Path) -> None:
    real = tmp_path.resolve() / "dotfiles/hooks.json"
    real.parent.mkdir()
    link = tmp_path.resolve() / "hooks.json"
    link.symlink_to(real)
    state = tool.hook_state(listing([hook(real, "key", "sha256:aa")]), link)
    assert state == {"key": {"trusted_hash": "sha256:aa"}}


def test_hook_state_requires_a_hook_from_the_installed_file(tmp_path: Path) -> None:
    installed = tmp_path.resolve() / "hooks.json"
    other = tmp_path.resolve() / "other.json"
    with pytest.raises(RuntimeError, match="lists no hooks"):
        _ = tool.hook_state(listing([hook(other, "key", "sha256:aa")]), installed)


def test_hook_state_reports_a_hooks_file_that_does_not_load(tmp_path: Path) -> None:
    installed = tmp_path.resolve() / "hooks.json"
    errors = [{"path": str(installed), "message": "expected value at line 1"}]
    with pytest.raises(RuntimeError, match="expected value at line 1"):
        _ = tool.hook_state(listing([], errors), installed)


def test_hook_state_shows_the_warnings_when_no_hook_is_listed(tmp_path: Path) -> None:
    installed = tmp_path.resolve() / "hooks.json"
    warnings = [f"failed to parse hooks config {installed}: key must be a string"]
    with pytest.raises(RuntimeError, match="key must be a string"):
        _ = tool.hook_state(listing([], warnings=warnings), installed)


def test_hook_state_ignores_errors_in_other_files(tmp_path: Path) -> None:
    installed = tmp_path.resolve() / "hooks.json"
    errors = [{"path": str(tmp_path / "other.json"), "message": "broken"}]
    state = tool.hook_state(
        listing([hook(installed, "key", "sha256:aa")], errors), installed
    )
    assert state == {"key": {"trusted_hash": "sha256:aa"}}


@final
class StubCodex:
    def __init__(self, root: Path, monkeypatch: pytest.MonkeyPatch) -> None:
        self.state = root / "stub"
        self.state.mkdir()
        bin_dir = root / "bin"
        bin_dir.mkdir()
        executable = bin_dir / "codex"
        _ = executable.write_text(STUB.replace("PYTHON", sys.executable, 1))
        executable.chmod(0o755)
        monkeypatch.setenv("PATH", f"{bin_dir}{os.pathsep}{os.environ['PATH']}")
        monkeypatch.setenv("STUB_DIR", str(self.state))
        self.script({})

    def script(self, script: Json) -> None:
        script = {"replies": {}, **script}
        _ = self.state.joinpath("script.json").write_text(json.dumps(script))

    def received(self) -> list[Json]:
        log = self.state / "received.jsonl"
        return [cast("Json", json.loads(line)) for line in log.read_text().splitlines()]

    def launch(self) -> Json:
        return cast("Json", json.loads(self.state.joinpath("launch.json").read_text()))

    def methods(self) -> list[object]:
        return [message["method"] for message in self.received()]

    def batch_write(self) -> Json:
        (params,) = [
            message["params"]
            for message in self.received()
            if message["method"] == "config/batchWrite"
        ]
        return cast("Json", params)


@pytest.fixture
def codex(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> StubCodex:
    return StubCodex(tmp_path, monkeypatch)


@pytest.fixture
def codex_home(tmp_path: Path) -> Path:
    home = tmp_path.resolve() / "home/.codex"
    home.mkdir(parents=True)
    return home


def edit(key_path: str, value: object) -> Json:
    return {"keyPath": key_path, "value": value, "mergeStrategy": "upsert"}


def test_configure_trusts_the_installed_hooks(
    codex: StubCodex, codex_home: Path
) -> None:
    hooks_file = codex_home / "hooks.json"
    _ = hooks_file.write_text("{}")
    elsewhere = codex_home.parent / "src/repo/.codex/hooks.json"
    key = f"{hooks_file}:pre_tool_use:0:0"
    codex.script(
        {
            "replies": {
                "hooks/list": {
                    "result": listing(
                        [
                            hook(hooks_file, key, "sha256:new"),
                            hook(
                                elsewhere, f"{elsewhere}:pre_tool_use:0:0", "sha256:x"
                            ),
                        ]
                    )
                }
            }
        }
    )
    config_file = codex_home / "config.toml"
    _ = config_file.write_text('[auto_review]\nextra_policy = "Keep this."\n')

    tool.configure_codex(config_file, HOME)

    received = codex.received()
    assert [message.get("method") for message in received] == [
        "initialize",
        "initialized",
        "hooks/list",
        "config/batchWrite",
    ]
    assert received[0]["params"] == {"clientInfo": {"name": "dotfiles", "version": "1"}}
    assert "id" not in received[1]
    assert received[2]["params"] == {"cwds": [str(codex_home)]}
    assert codex.batch_write() == {
        "filePath": str(config_file),
        "edits": [
            edit("auto_review.extra_policy", f"Keep this.\n\n{tool.LGTMCP_POLICY}"),
            edit("mcp_servers.lgtmcp.tools.review_only.approval_mode", "approve"),
            edit("mcp_servers.lgtmcp.tools.review_and_commit.approval_mode", "approve"),
            edit("approvals_reviewer", "auto_review"),
            edit("features.worktrees", True),  # noqa: FBT003
            edit("features.network_proxy", True),  # noqa: FBT003
            edit("default_permissions", "dev"),
            edit("permissions.dev", DEVELOPMENT_PERMISSIONS),
            edit("projects", {PROJECTS: {"trust_level": "trusted"}}),
            edit("hooks.state", {key: {"trusted_hash": "sha256:new"}}),
        ],
    }


def test_configure_points_the_app_server_at_the_config_directory(
    codex: StubCodex, codex_home: Path
) -> None:
    tool.configure_codex(codex_home / "config.toml", HOME)
    launch = codex.launch()
    argv = cast("list[str]", launch["argv"])
    assert argv == ["app-server"]
    assert launch["codex_home"] == str(codex_home)


def test_configure_accepts_a_legacy_tui_setting(codex_home: Path) -> None:
    if shutil.which("codex") is None:
        pytest.skip("Codex CLI is not installed")
    config_file = codex_home / "config.toml"
    _ = config_file.write_text(
        '[mcp_servers.lgtmcp]\ncommand = "lgtmcp"\n\n[tui]\nwhimsy = false\n'
    )

    tool.configure_codex(config_file, HOME)

    config = tomllib.loads(config_file.read_text())
    assert config["tui"]["whimsy"] is False
    assert config["approvals_reviewer"] == "auto_review"
    assert config["auto_review"]["extra_policy"] == tool.LGTMCP_POLICY


@pytest.mark.parametrize("sandbox_mode", [None, "danger-full-access"])
def test_configure_permissions_with_the_real_cli(
    codex_home: Path, sandbox_mode: str | None
) -> None:
    if shutil.which("codex") is None:
        pytest.skip("Codex CLI is not installed")
    config_file = codex_home / "config.toml"
    config_text = (
        'default_permissions = "audit"\n'
        '[mcp_servers.lgtmcp]\ncommand = "lgtmcp"\n'
        "[features]\nhooks = false\n"
        '[permissions.dev]\ndescription = "Keep this"\n'
        '[permissions.dev.filesystem]\n"~/.ssh" = "deny"\n'
        '[permissions.dev.network.domains]\n"example.com" = "deny"\n'
        '[permissions.audit]\nextends = ":read-only"\n'
    )
    if sandbox_mode:
        config_text = f'sandbox_mode = "{sandbox_mode}"\n{config_text}'
    _ = config_file.write_text(config_text)

    tool.configure_codex(config_file, HOME)

    config = tomllib.loads(config_file.read_text())
    assert config.get("sandbox_mode") == sandbox_mode
    assert config["default_permissions"] == "dev"
    assert config["features"] == {
        "hooks": False,
        "worktrees": True,
        "network_proxy": True,
    }
    assert config["permissions"]["audit"] == {"extends": ":read-only"}
    assert config["permissions"]["dev"] == {
        "description": "Keep this",
        "extends": ":workspace",
        "filesystem": {"~/.ssh": "deny", "~/.cache/uv": "write"},
        "network": {
            "enabled": True,
            "allow_local_binding": True,
            "domains": {
                "example.com": "deny",
                "localhost": "allow",
                "127.0.0.1": "allow",
            },
        },
    }
    first = config_file.read_bytes()
    tool.configure_codex(config_file, HOME)
    assert config_file.read_bytes() == first


def test_configure_without_installed_hooks_does_not_list_them(
    codex: StubCodex, codex_home: Path
) -> None:
    tool.configure_codex(codex_home / "config.toml", HOME)
    assert codex.methods() == ["initialize", "initialized", "config/batchWrite"]
    edits = cast("list[Json]", codex.batch_write()["edits"])
    assert "hooks.state" not in [item["keyPath"] for item in edits]


def test_configure_does_not_trust_hooks_that_are_disabled(
    codex: StubCodex, codex_home: Path
) -> None:
    _ = codex_home.joinpath("hooks.json").write_text("{}")
    config_file = codex_home / "config.toml"
    _ = config_file.write_text("[features]\nhooks = false\n")
    codex.script({"replies": {"hooks/list": {"result": listing([])}}})
    tool.configure_codex(config_file, HOME)
    assert codex.methods() == ["initialize", "initialized", "config/batchWrite"]
    edits = cast("list[Json]", codex.batch_write()["edits"])
    assert "hooks.state" not in [item["keyPath"] for item in edits]


def test_configure_leaves_an_existing_project_decision_alone(
    codex: StubCodex, codex_home: Path
) -> None:
    config_file = codex_home / "config.toml"
    _ = config_file.write_text(f'[projects."{PROJECTS}"]\ntrust_level = "untrusted"\n')
    tool.configure_codex(config_file, HOME)
    edits = cast("list[Json]", codex.batch_write()["edits"])
    assert "projects" not in [item["keyPath"] for item in edits]


def test_configure_fails_when_no_hook_is_listed_from_the_installed_file(
    codex: StubCodex, codex_home: Path
) -> None:
    _ = codex_home.joinpath("hooks.json").write_text("{}")
    codex.script({"replies": {"hooks/list": {"result": listing([])}}})
    with pytest.raises(RuntimeError, match="lists no hooks"):
        tool.configure_codex(codex_home / "config.toml", HOME)
    assert "config/batchWrite" not in codex.methods()


def test_configure_surfaces_a_rejected_write(
    codex: StubCodex, codex_home: Path
) -> None:
    codex.script(
        {"replies": {"config/batchWrite": {"error": {"code": -32600, "message": "no"}}}}
    )
    with pytest.raises(RuntimeError, match=r"rejected config/batchWrite.*'no'"):
        tool.configure_codex(codex_home / "config.toml", HOME)


def test_configure_fails_when_the_app_server_exits(
    codex: StubCodex, codex_home: Path
) -> None:
    codex.script({"exit_on": "config/batchWrite"})
    with pytest.raises(RuntimeError, match="exited before answering config/batchWrite"):
        tool.configure_codex(codex_home / "config.toml", HOME)


def test_configure_skips_unsolicited_messages(
    codex: StubCodex, codex_home: Path
) -> None:
    codex.script(
        {
            "before": [
                {"method": "configWarning", "params": {"summary": "careful"}},
                {"id": 1, "method": "client/request", "params": {}},
                {"id": 2, "method": "client/request", "params": {}},
                {"id": 99, "result": {}},
            ]
        }
    )
    tool.configure_codex(codex_home / "config.toml", HOME)
    assert codex.methods() == ["initialize", "initialized", "config/batchWrite"]


def test_command_line_uses_the_home_directory(
    codex: StubCodex, codex_home: Path, tmp_path: Path
) -> None:
    config_file = codex_home / "config.toml"
    _ = subprocess.run(
        [sys.executable, str(TOOL), str(config_file)],
        env={**os.environ, "HOME": str(tmp_path / "home")},
        check=True,
        timeout=30,
    )
    edits = cast("list[Json]", codex.batch_write()["edits"])
    (projects,) = [item for item in edits if item["keyPath"] == "projects"]
    assert projects["value"] == {
        str(tmp_path / "home/src/github.com/shields/dotfiles"): {
            "trust_level": "trusted"
        }
    }
