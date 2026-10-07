#!/bin/bash

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

# Make this machine a throwaway environment: Claude Code and Codex run in it
# with no sandbox and no prompts, and the deny rules and git_guard.py still
# stop what would change state outside it. Run as root after provision.sh.
#
# Claude's policy is a managed settings file, which no other settings file
# overrides. Because its ask rules and hooks would prompt even in bypass mode,
# managed settings are the only source of permission rules and hooks, and they
# carry the deny rules, the git_guard.py hook and the status line of this
# repository's .claude/settings.json. Codex's policy is its system config,
# which ~/.codex/config.toml overrides key by key.
#
# It also installs the timer that renews the GitHub App token of bin/limavm's
# VMs. The unit is a system unit, since a user unit runs only while the user is
# logged in or lingering, and it does nothing until an auth record exists.

set -euo pipefail

usage="usage: sudo provision/throwaway.sh [--etc-dir DIR] [--user NAME | --no-github-timer]"

die() {
    printf 'throwaway.sh: %s\n' "$*" >&2
    exit 1
}

etc=/etc
user=${SUDO_USER:-}
timer=1
while (($#)); do
    case $1 in
    -h | --help)
        printf '%s\n' "$usage"
        exit 0
        ;;
    --etc-dir)
        [[ $# -ge 2 && -n $2 ]] || die "$usage"
        etc=$2
        shift 2
        ;;
    --user)
        [[ $# -ge 2 && -n $2 ]] || die "$usage"
        user=$2
        shift 2
        ;;
    --no-github-timer)
        timer=0
        shift
        ;;
    *) die "$usage" ;;
    esac
done
if [[ $etc == /etc && $(id -u) -ne 0 ]]; then
    die "must run as root; use: sudo $0"
fi
if ((timer)); then
    [[ -n $user ]] || die "cannot tell which user runs the GitHub token timer; pass --user NAME, or --no-github-timer"
    [[ $user =~ ^[A-Za-z_][A-Za-z0-9._-]*$ ]] || die "invalid user name: $user"
fi
command -v jq >/dev/null || die "jq is not installed"

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
settings=$script_dir/../.claude/settings.json
[[ -f $settings ]] || die "$settings does not exist"

umask 022
claude_dir=$etc/claude-code
codex_dir=$etc/codex
mkdir -p "$claude_dir" "$codex_dir"

# Each file is written in full here, then renamed into place, so a reader never
# sees a partial policy. mktemp makes the directory private, and the files
# inside it need the mode that every user can read.
work=$(mktemp -d "$etc/.throwaway.XXXXXX")
trap 'rm -rf "$work"' EXIT

jq '
  def guard: [.hooks.PreToolUse[]? | select(any(.hooks[]?; (.command // "") | contains("git_guard.py")))];
  if (guard | length) == 0 then error("settings.json registers no git_guard.py hook") else . end
  | if ((.permissions.deny // []) | length) == 0 then error("settings.json has no permissions.deny rules") else . end
  | {
      permissions: {defaultMode: "bypassPermissions", deny: .permissions.deny},
      allowManagedPermissionRulesOnly: true,
      allowManagedHooksOnly: true,
      skipDangerousModePermissionPrompt: true,
      sandbox: {enabled: false},
      hooks: {PreToolUse: guard}
    } + (if .statusLine then {statusLine: .statusLine} else {} end)
' "$settings" >"$work/managed-settings.json" || die "cannot build the managed settings from $settings"

cat >"$work/config.toml" <<'EOF'
# Written by provision/throwaway.sh. ~/.codex/config.toml overrides these.
approval_policy = "never"
sandbox_mode = "danger-full-access"
EOF

# Codex parses a rules file as Starlark, so every prose line in this one
# starts with #; the first three lines are one comment, wrapped.
cat >"$work/throwaway.rules" <<'EOF'
# Written by provision/throwaway.sh. Claude Code denies every gh api request
# that names a method, wherever the flag stands; a Codex rule matches only the
# words that start a command, so it stops the commands that put the flag first.
GH = ["gh", "/usr/bin/gh", "/home/linuxbrew/.linuxbrew/bin/gh"]

prefix_rule(
    pattern = [GH, "api", ["-X", "--method"]],
    decision = "forbidden",
    justification = "The user performs all GitHub writes. Use gh api for reads only.",
    match = ["gh api -X POST repos/example/repo/issues", "gh api --method DELETE repos/example/repo/git/refs/heads/x", "/usr/bin/gh api -X PUT repos/example/repo/contents/file"],
    not_match = ["gh api repos/example/repo/issues", "gh pr list"],
)
EOF

cat >"$work/dotfiles-throwaway" <<'EOF'
This machine is a throwaway environment. provision/throwaway.sh wrote this
file, and the interactive shell reads its presence to start Claude Code
without the sandbox and permission prompts. The agents' policy is in
claude-code/managed-settings.json and codex/config.toml beside it.
EOF

if ((timer)); then
    cat >"$work/github-app-refresh.service" <<EOF
[Unit]
Description=Renew the GitHub App token of $user
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
User=$user
Environment=PATH=/home/linuxbrew/.linuxbrew/bin:/usr/local/bin:/usr/bin:/bin
ExecCondition=/bin/sh -c 'test -e "\$\$HOME/.config/github-app/auth.json"'
ExecStart=/bin/sh -c 'exec "\$\$HOME/bin/github_app_token.py" refresh-if-needed'
NoNewPrivileges=yes
PrivateTmp=yes
ProtectSystem=full
EOF
    cat >"$work/github-app-refresh.timer" <<'EOF'
[Unit]
Description=Renew the GitHub App token every few minutes

[Timer]
OnBootSec=1min
OnCalendar=*:0/5
Persistent=yes

[Install]
WantedBy=timers.target
EOF
fi

chmod 644 "$work"/*
mkdir -p "$codex_dir/rules"
mv -f "$work/managed-settings.json" "$claude_dir/managed-settings.json"
mv -f "$work/config.toml" "$codex_dir/config.toml"
mv -f "$work/throwaway.rules" "$codex_dir/rules/throwaway.rules"
mv -f "$work/dotfiles-throwaway" "$etc/dotfiles-throwaway"

if ((timer)); then
    units=$etc/systemd/system
    mkdir -p "$units/timers.target.wants"
    mv -f "$work/github-app-refresh.service" "$units/github-app-refresh.service"
    mv -f "$work/github-app-refresh.timer" "$units/github-app-refresh.timer"
    ln -sfn ../github-app-refresh.timer "$units/timers.target.wants/github-app-refresh.timer"
    if [[ $etc == /etc && -d /run/systemd/system ]]; then
        systemctl daemon-reload
    fi
fi
