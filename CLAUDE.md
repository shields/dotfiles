# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

- **Build**: `npm run build` (TypeScript compilation)
- **Tools**: `npm run color-palette` (Color palette generation)
- **System Setup**: `./provision.sh` (Full macOS setup)
- **Emacs Setup**: `emacs --batch --script .emacs.d/provision.el`

## Code Style

- **TypeScript**: Modern ES2020 target, strict types, Node modules
- **Python**: Type annotations required, Python 3.13+, Ruff formatting
- **Emacs Lisp**: Lexical binding, use-package based, use keymap-set
- **Shell Scripts**: set -euo pipefail, prefer absolute paths

## Package Management

- Use `yarn` for JavaScript/TypeScript
- Use `uv` for Python packages (never pip). Even without pyproject.toml, use `uv venv` and `uv pip`
- Use `brew` for system packages

## Project Conventions

- Do not optimize Emacs startup time - use emacsclient
- Tools go in `tools/` directory with descriptive names
- Configuration files at root level
- Prefer tree-sitter modes when available
- Always include proper type annotations
