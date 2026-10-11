#!/bin/bash

set -euo pipefail

[[ $(id -u) -eq 0 ]] || {
    printf '%s\n' 'cloudflare-agent.sh: run as root' >&2
    exit 1
}

settings=/etc/claude-code/managed-settings.json
[[ -f $settings ]] || {
    printf '%s\n' 'cloudflare-agent.sh: managed settings are missing' >&2
    exit 1
}
managed=$(mktemp /etc/claude-code/.managed-settings.XXXXXX)
trap 'rm -f "$managed"' EXIT
jq '.permissions.deny |= map(select(. as $entry | [
    "Bash(git push*)",
    "Bash(git -C * push*)",
    "Bash(git -c * push*)",
    "Bash(git --git-dir* push*)",
    "Bash(git --work-tree* push*)"
  ] | index($entry) | not))' "$settings" >"$managed"
chmod 644 "$managed"
mv -f "$managed" "$settings"

rm -f /home/shields/.codex/rules/no-push.rules
sudo -H -u shields git config --global credential.https://github.com.helper '!/home/shields/bin/github-gateway-credential'
printf '%s\n' cloudflare >/etc/dotfiles-github-gateway
chmod 644 /etc/dotfiles-github-gateway
