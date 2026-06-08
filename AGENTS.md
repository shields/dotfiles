# AGENTS.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

- **Build/Lint/Test/Format**: `make build`, `make lint`, `make test`, `make fmt`
- **Type Check**: `basedpyright` and `uv run ty check` (Python type checking)
- **Tools**: `bun run tools/color-palette.ts` (Color palette generation)
- **System Setup**: `./provision.sh` (Full macOS setup)
- **Emacs Setup**: `emacs --batch --script .emacs.d/provision.el`

## Deployment

`provision.sh` installs the dotfiles by piping them through `tar` into `$HOME`:
`bin/`, `Library/`, and every git-tracked path starting with `.` (e.g.
`.claude/`, `.zshrc`). Files are copied, not symlinked, so this repo is the
source of truth; edits take effect only after running `./provision.sh`. Because
`.claude/` is copied, `.claude/CLAUDE.md` here is installed as the global
`~/.claude/CLAUDE.md`.

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
