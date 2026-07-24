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

# Shell scripts to lint and format with shellcheck and shfmt. zsh files
# (.zshrc, .zprofile, .zsh.d/*.zsh, tests/*.zsh) and vendored files
# (.iTerm2/*) are excluded because neither tool supports zsh.
SHELL_SOURCES = \
	.bashrc \
	.bash_profile \
	.profile \
	.claude/statusline.sh \
	provision.sh \
	tools/create_nerd_andale_mono.sh \
	bin/$$ \
	bin/docker-prune \
	bin/ghfork \
	bin/git-add-upstream \
	bin/pager

build:
	bun run tsc --noEmit

test:
	zsh tests/test_gcl.zsh
	zsh tests/test_git_add_upstream.zsh
	zsh tests/test_ghfork.zsh
	uv run python tests/test_dependency_guard.py

lint:
	bun run eslint .
	ruff check
	uv run ty check
	basedpyright
	shellcheck --exclude=SC1091 $(SHELL_SOURCES)
	shfmt -i 4 -d $(SHELL_SOURCES)

fmt:
	bun run prettier --write --ignore-path .gitignore "**/*.ts" "**/*.json" "**/*.md"
	ruff format
	shfmt -i 4 -w $(SHELL_SOURCES)

run:
	bun run tools/color-palette.ts
