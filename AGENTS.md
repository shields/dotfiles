<!--
Copyright © 2025-2026 Michael Shields

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

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
  - In Zsh, never use lowercase `path` as a local or general-purpose variable:
    it is a special array tied to `PATH`, so shadowing it can break command
    lookup and `chpwd` hooks. Use a descriptive name such as `worktree_path`.

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
