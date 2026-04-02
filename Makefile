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
