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

.PHONY: build test test-linux lint fmt run bench bench-startup

# Shell scripts to lint and format with shellcheck and shfmt. zsh files
# (.zshrc, .zprofile, .zsh.d/*.zsh, tests/*.zsh) and vendored files
# (.iTerm2/*) are excluded because neither tool supports zsh.
SHELL_SOURCES = \
	.bashrc \
	.bash_profile \
	.profile \
	.claude/statusline.sh \
	provision.sh \
	provision/linux-system.sh \
	provision/macos.sh \
	provision/modules.sh \
	provision/reset-identity.sh \
	provision/throwaway.sh \
	tools/create_nerd_andale_mono.sh \
	tools/create_nerd_commit_mono.sh \
	tools/stage_tree.sh \
	bin/$$ \
	bin/docker-prune \
	bin/ghfork \
	bin/git-add-upstream \
	bin/limavm \
	bin/pager \
	bin/setup-secrets

build:
	bun run tsc --noEmit

test:
	zsh tests/test_gcl.zsh
	zsh tests/test_git_add_upstream.zsh
	zsh tests/test_ghfork.zsh
	zsh tests/test_limavm.zsh
	zsh tests/test_setup_secrets.zsh
	zsh tests/test_wt.zsh
	uv run pytest

MODULES ?= dev
LINUX_IMAGE ?= dotfiles-linux-test

test-linux:
	@set -eu; \
	stage=$$(mktemp -d "$${TMPDIR:-/tmp}/dotfiles-test-linux.XXXXXX"); \
	trap 'rm -rf "$$stage"' EXIT; \
	trap 'exit 1' HUP INT TERM; \
	tools/stage_tree.sh "$$stage/tree"; \
	docker build --build-arg "MODULES=$(MODULES)" -t "$(LINUX_IMAGE)" -f cloudflare/Dockerfile "$$stage/tree"; \
	docker run --rm "$(LINUX_IMAGE)" zsh -c 'bun install --frozen-lockfile && make test lint'

lint:
	bun run eslint .
	uv run ruff check
	uv run ty check
	basedpyright
	shellcheck --exclude=SC1091 $(SHELL_SOURCES)
	shfmt -i 4 -d $(SHELL_SOURCES)

fmt:
	bun run prettier --write "**/*.ts" "**/*.json" "**/*.md"
	uv run ruff format
	shfmt -i 4 -w $(SHELL_SOURCES)

run:
	bun run tools/color-palette.ts

bench:
	zsh tools/bench_wt.zsh

bench-startup:
	zsh tools/bench_startup.zsh -p
