# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

- **Build/Lint/Test/Format**: `make build`, `make lint`, `make test`, `make fmt`
- **Type Check**: `basedpyright` and `uv run ty check` (Python type checking)
- **Tools**: `bun run tools/color-palette.ts` (Color palette generation)
- **System Setup**: `./provision.sh` (Full macOS setup)
- **Emacs Setup**: `emacs --batch --script .emacs.d/provision.el`

## Code Style

- **TypeScript**: ESNext target, strict types, ESLint + Prettier, Node modules
- **Python**: Type annotations required, Python 3.14+, Ruff formatting
- **Emacs Lisp**: Lexical binding, use-package based, use keymap-set
- **Shell Scripts**: set -euo pipefail, prefer absolute paths

## Package Management

- Use `bun` for JavaScript/TypeScript
- Use `uv` for Python packages (never pip). Even without pyproject.toml, use `uv venv` and `uv pip`
- Use `brew` for system packages

## Project Conventions

- Do not optimize Emacs startup time - use emacsclient
- Tools go in `tools/` directory with descriptive names
- Configuration files at root level
- Prefer tree-sitter modes when available
- Always include proper type annotations
