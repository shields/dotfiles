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

.PHONY: build test lint fmt run

build:
	bun run tsc --noEmit

test:
	zsh tests/test_gcl.zsh
	zsh tests/test_git_add_upstream.zsh
	zsh tests/test_ghfork.zsh

lint:
	bun run eslint .
	ruff check
	uv run ty check
	basedpyright

fmt:
	bun run prettier --write --ignore-path .gitignore "**/*.ts" "**/*.json" "**/*.md"
	ruff format

run:
	bun run tools/color-palette.ts
