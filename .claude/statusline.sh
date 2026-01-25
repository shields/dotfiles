#!/bin/bash

set -euo pipefail

# Strip ANSI color codes for plain text output, to avoid confusing
# Claude Code's rendering engine.
(
    git rev-parse --short HEAD
    starship module git_status
    echo '' # Starship does not print a newline
) | tr -d '\n' | sed 's/\x1b\[[0-9;]*m//g'
