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

import argparse
import asyncio
import json
import os
import tomllib
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, cast, final

if TYPE_CHECKING:
    from collections.abc import AsyncGenerator

type Json = dict[str, object]

LGTMCP_POLICY = (
    "LGTMCP is an approved code-review service. Google's Gemini is a trusted\n"
    "destination for repository code and diffs sent through\n"
    "mcp__lgtmcp__review_only and mcp__lgtmcp__review_and_commit.\n"
    "Allow these transfers for code review and authorized commits.\n"
    "This authorization excludes credentials and unrelated private data."
)

# Codex matches a project key against a repository's root exactly, so a
# wildcard key trusts nothing.
TRUSTED_PROJECT = "src/github.com/shields/dotfiles"


@final
class AppServer:
    def __init__(self, process: asyncio.subprocess.Process) -> None:
        if process.stdin is None or process.stdout is None:
            msg = "Codex app server requires stdin and stdout pipes"
            raise RuntimeError(msg)
        self.stdin = process.stdin
        self.stdout = process.stdout
        self.next_id = 1

    async def send(self, message: Json) -> None:
        self.stdin.write((json.dumps(message) + "\n").encode())
        await self.stdin.drain()

    async def notify(self, method: str) -> None:
        await self.send({"method": method})

    async def request(self, method: str, params: Json) -> Json:
        request_id = self.next_id
        self.next_id += 1
        await self.send({"id": request_id, "method": method, "params": params})
        while line := await self.stdout.readline():
            message = cast("Json", json.loads(line))
            if "method" in message or message.get("id") != request_id:
                continue
            if "error" in message:
                msg = f"Codex app server rejected {method}: {message['error']}"
                raise RuntimeError(msg)
            return cast("Json", message["result"])
        msg = f"Codex app server exited before answering {method}"
        raise RuntimeError(msg)


@asynccontextmanager
async def app_server(codex_home: Path) -> AsyncGenerator[AppServer]:
    # Do not move sqlite_home to an empty directory: the server then rebuilds its
    # state from every saved session before it answers initialize, which takes
    # minutes when ~/.codex holds gigabytes of them.
    process = await asyncio.create_subprocess_exec(
        "codex",
        "app-server",
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        env={**os.environ, "CODEX_HOME": str(codex_home)},
    )
    try:
        server = AppServer(process)
        _ = await server.request(
            "initialize", {"clientInfo": {"name": "dotfiles", "version": "1"}}
        )
        await server.notify("initialized")
        yield server
    finally:
        if process.returncode is None:
            process.terminate()
        try:
            _ = await asyncio.wait_for(process.wait(), timeout=5)
        except TimeoutError:
            process.kill()
            _ = await process.wait()


def table(config: Json, name: str) -> Json:
    value = config.get(name, {})
    if not isinstance(value, dict):
        msg = f"{name} must be a TOML table"
        raise TypeError(msg)
    return cast("Json", value)


def settings(config: Json, home: Path) -> Json:
    extra_policy = table(config, "auto_review").get("extra_policy", "")
    if not isinstance(extra_policy, str):
        msg = "auto_review.extra_policy must be a string"
        raise TypeError(msg)
    if LGTMCP_POLICY not in extra_policy:
        extra_policy = (
            f"{extra_policy}\n\n{LGTMCP_POLICY}" if extra_policy else LGTMCP_POLICY
        )
    result: Json = {
        "auto_review.extra_policy": extra_policy,
        "mcp_servers.lgtmcp.tools.review_only.approval_mode": "approve",
        "mcp_servers.lgtmcp.tools.review_and_commit.approval_mode": "approve",
        "approvals_reviewer": "auto_review",
        "features.worktrees": True,
    }
    # A project key contains dots, so the whole table is the edit's key path.
    project = str(home / TRUSTED_PROJECT)
    if project not in table(config, "projects"):
        result["projects"] = {project: {"trust_level": "trusted"}}
    return result


def hook_state(listing: Json, hooks_file: Path) -> Json:
    hooks_file = hooks_file.resolve()
    state: Json = {}
    for entry in cast("list[Json]", listing["data"]):
        for error in cast("list[dict[str, str]]", entry["errors"]):
            if Path(error["path"]).resolve() == hooks_file:
                msg = f"Codex cannot load {hooks_file}: {error['message']}"
                raise RuntimeError(msg)
        for hook in cast("list[dict[str, str]]", entry["hooks"]):
            if Path(hook["sourcePath"]).resolve() == hooks_file:
                state[hook["key"]] = {"trusted_hash": hook["currentHash"]}
    if not state:
        warnings = [
            warning
            for entry in cast("list[Json]", listing["data"])
            for warning in cast("list[str]", entry["warnings"])
        ]
        detail = f": {'; '.join(warnings)}" if warnings else ""
        msg = f"Codex lists no hooks from {hooks_file}{detail}"
        raise RuntimeError(msg)
    return state


def edits(values: Json) -> list[Json]:
    return [
        {"keyPath": key, "value": value, "mergeStrategy": "upsert"}
        for key, value in values.items()
    ]


async def write_config(config_file: Path, values: Json, *, hooks_enabled: bool) -> None:
    hooks_file = config_file.parent / "hooks.json"
    async with asyncio.timeout(30), app_server(config_file.parent) as server:
        if hooks_enabled and hooks_file.exists():
            listing = await server.request(
                "hooks/list", {"cwds": [str(config_file.parent)]}
            )
            values = {**values, "hooks.state": hook_state(listing, hooks_file)}
        _ = await server.request(
            "config/batchWrite",
            {"filePath": str(config_file), "edits": edits(values)},
        )


def configure_codex(config_file: Path, home: Path) -> None:
    config_file = config_file.resolve()
    config = (
        cast("Json", tomllib.loads(config_file.read_text(encoding="utf-8")))
        if config_file.exists()
        else {}
    )
    # Codex lists no hooks while features.hooks is false, and hook_state rejects
    # that.
    hooks_enabled = table(config, "features").get("hooks") is not False
    asyncio.run(
        write_config(config_file, settings(config, home), hooks_enabled=hooks_enabled)
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Configure Codex: settings, hook trust, LGTMCP authorization"
    )
    _ = parser.add_argument("config_file", type=Path)
    args = cast("dict[str, Path]", vars(parser.parse_args()))
    configure_codex(args["config_file"], Path.home())
