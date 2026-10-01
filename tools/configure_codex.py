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
import tempfile
import tomllib
from pathlib import Path
from typing import cast

LGTMCP_POLICY = (
    "LGTMCP is an approved code-review service. Google's Gemini is a trusted\n"
    "destination for repository code and diffs sent through\n"
    "mcp__lgtmcp__review_only and mcp__lgtmcp__review_and_commit.\n"
    "Allow these transfers for code review and authorized commits.\n"
    "This authorization excludes credentials and unrelated private data."
)


async def request(
    process: asyncio.subprocess.Process,
    request_id: int,
    method: str,
    params: dict[str, object],
) -> None:
    if process.stdin is None or process.stdout is None:
        msg = "Codex app server requires stdin and stdout pipes"
        raise RuntimeError(msg)
    process.stdin.write(
        (
            json.dumps({"id": request_id, "method": method, "params": params}) + "\n"
        ).encode(),
    )
    await process.stdin.drain()
    while line := await process.stdout.readline():
        response = cast("dict[str, object]", json.loads(line))
        if response.get("id") == request_id:
            if "error" in response:
                raise RuntimeError(response["error"])
            return
    msg = f"Codex app server exited before answering {method}"
    raise RuntimeError(msg)


def configure_codex(config_file: Path) -> None:
    config_file = config_file.resolve()
    config = (
        cast("dict[str, object]", tomllib.loads(config_file.read_text()))
        if config_file.exists()
        else {}
    )
    auto_review = config.get("auto_review", {})
    if not isinstance(auto_review, dict):
        msg = "auto_review must be a TOML table"
        raise TypeError(msg)
    extra_policy = cast("dict[str, object]", auto_review).get("extra_policy", "")
    if not isinstance(extra_policy, str):
        msg = "auto_review.extra_policy must be a string"
        raise TypeError(msg)
    if LGTMCP_POLICY not in extra_policy:
        extra_policy = (
            f"{extra_policy}\n\n{LGTMCP_POLICY}" if extra_policy else LGTMCP_POLICY
        )
    settings = {
        "auto_review.extra_policy": extra_policy,
        "mcp_servers.lgtmcp.tools.review_only.approval_mode": "approve",
        "mcp_servers.lgtmcp.tools.review_and_commit.approval_mode": "approve",
    }
    asyncio.run(write_config(config_file, settings))


async def write_config(config_file: Path, settings: dict[str, str]) -> None:
    with tempfile.TemporaryDirectory(prefix="codex-config-") as state_dir:
        process = await asyncio.create_subprocess_exec(
            "codex",
            "app-server",
            "--strict-config",
            "-c",
            f"sqlite_home={json.dumps(state_dir)}",
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
        )
        try:
            async with asyncio.timeout(30):
                await request(
                    process,
                    1,
                    "initialize",
                    {"clientInfo": {"name": "dotfiles", "version": "1"}},
                )
                if process.stdin is None:
                    msg = "Codex app server closed stdin"
                    raise RuntimeError(msg)
                process.stdin.write(b'{"method":"initialized"}\n')
                await request(
                    process,
                    2,
                    "config/batchWrite",
                    {
                        "filePath": str(config_file),
                        "edits": [
                            {"keyPath": key, "value": value, "mergeStrategy": "upsert"}
                            for key, value in settings.items()
                        ],
                    },
                )
        finally:
            if process.returncode is None:
                process.terminate()
            try:
                _ = await asyncio.wait_for(process.wait(), timeout=5)
            except TimeoutError:
                process.kill()
                _ = await process.wait()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Authorize LGTMCP code reviews in Codex"
    )
    _ = parser.add_argument("config_file", type=Path)
    args = cast("dict[str, Path]", vars(parser.parse_args()))
    configure_codex(args["config_file"])
