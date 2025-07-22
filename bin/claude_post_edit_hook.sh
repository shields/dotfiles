#!/bin/bash

set -euo pipefail

f="$(jq -r '.tool_input.file_path')"

case "$f" in
*.py)
    ruff format -- "$f"
    ;;
*.js | *.json | *.md | *.ts | *.yaml | *.yml)
    prettier --write "$f"
    ;;
*.rs)
    rustfmt -- "$f"
    ;;
*.go)
    gofmt -w "$f"
    ;;
*.sh)
    shfmt --write "$f"
    ;;
esac

exit 0
