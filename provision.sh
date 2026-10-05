#!/bin/bash

set -euo pipefail

# Copyright © 2018-2026 Michael Shields
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

cd "$(dirname "$0")"

uname_s=$(uname -s)
case $uname_s in
Darwin) os=macos ;;
*)
    echo "provision.sh: unsupported OS $uname_s; only macOS is supported" >&2
    exit 1
    ;;
esac

if [[ $os == macos ]]; then
    source provision/macos.sh
fi

# Copy these files.
git ls-files -- '.*' | tar cf - -T - bin Library | (cd "$HOME" && tar xvf -)

{
    cat .agents/AGENTS.md
    printf '\n'
    cat .codex/instructions.md
} >"$HOME/.codex/AGENTS.md"

# Set email address in .gitconfig. Do this early so we don't leave it missing.
if [[ "$(whoami)" == shields ]] && ! (profiles status -type enrollment | grep -q ': Yes'); then
    git config --global user.email shields@msrl.com
    git config --global github.user shields # For Magit Forge
fi

# Install Homebrew and Xcode (which will take tens of minutes).
# Use path selection logic from https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
# We will install full Xcode later from the Mac App Store.
UNAME_MACHINE="$(/usr/bin/uname -m)"
if [[ ${UNAME_MACHINE} == "arm64" ]]; then
    HOMEBREW_PREFIX="/opt/homebrew"
    HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}"
else
    HOMEBREW_PREFIX="/usr/local"
    HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}/Homebrew"
fi
if [[ ! -d $HOMEBREW_REPOSITORY ]]; then
    # CI=1 suppresses confirmation prompts.
    CI=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"

brew analytics off

if [[ $os == macos ]]; then
    macos_preflight
fi

# Oh My Zsh installation. This is the interesting part of
# https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh
if [[ ! -d "$HOME/.oh-my-zsh" ]]; then
    git clone --depth=1 https://github.com/ohmyzsh/ohmyzsh "$HOME/.oh-my-zsh"
fi
"$HOME/.oh-my-zsh/tools/upgrade.sh" -v minimal

# Install or update the deferred startup scheduler.
if [[ ! -d "$HOME/.local/share/zsh-defer" ]]; then
    git clone --depth=1 https://github.com/romkatv/zsh-defer "$HOME/.local/share/zsh-defer"
else
    git -C "$HOME/.local/share/zsh-defer" pull --ff-only
fi

# Install or update fzf-tab plugin
if [[ ! -d "$HOME/.oh-my-zsh/custom/plugins/fzf-tab" ]]; then
    git clone --depth=1 https://github.com/Aloxaf/fzf-tab "$HOME/.oh-my-zsh/custom/plugins/fzf-tab"
else
    (cd "$HOME/.oh-my-zsh/custom/plugins/fzf-tab" && git pull)
fi

# Install or update git-prompt-watcher plugin
if [[ ! -d "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher" ]]; then
    git clone --depth=1 https://github.com/shields/git-prompt-watcher "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher"
else
    (cd "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher" && git pull)
fi

brew update

# Homebrew bundle sync. Update using `brew bundle dump -f --no-describe`.
# Keep upgrades below so Chrome can be excluded from cask upgrades.
brew bundle --force --no-upgrade | (grep -v '^Using ' || true)
brew bundle cleanup --force
# Homebrew upgrades. Run formulas and casks separately to prevent whiny messages.
brew upgrade --formula --yes
# Suppress upgrade of Chrome since it doesn't like to be upgraded while running.
brew outdated --greedy-auto-updates --cask --quiet | sed '/^google-chrome$/d' | xargs -r brew upgrade --cask --yes
brew autoremove
brew cleanup --prune=all

uv cache prune

go clean -modcache

bin/docker-prune

if [[ $os == macos ]]; then
    macos_xcode
    macos_login_shell
fi

# Plugins!
export PIP_DISABLE_PIP_VERSION_CHECK=1
datasette install --upgrade datasette-cluster-map | (grep -v '^Requirement already satisfied:' || true)
llm install --upgrade llm-{gemini,anthropic,perplexity,cmd,openai-plugin} | (grep -v '^Requirement already satisfied:' || true)
gcloud --quiet components update

if [[ $os == macos ]]; then
    macos_defaults
fi

# Emacs setup
emacs --batch --script .emacs.d/provision.el
if [[ $os == macos ]]; then
    macos_emacs_app
fi

# rustup
rustup default stable >/dev/null

# Bootstrap TLS trust to GitHub SSH trust.
if [ ! -f "$HOME/.ssh/known_hosts" ] || ! grep -q '^github\.com ' "$HOME/.ssh/known_hosts"; then
    mkdir -p "$HOME/.ssh"
    curl -s -L \
        -H "Accept: application/vnd.github+json" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        https://api.github.com/meta |
        jq -r '.ssh_keys[]' |
        sed -e 's/^/github.com /' >>"$HOME/.ssh/known_hosts"
fi

# Go setup. Tools (goimports, etc.) are installed to ~/bin via the Brewfile's
# `go` entries; only Go's own settings belong here.
go telemetry on

# Globally available MCP servers for Claude Code and Codex.
add_user_mcp_server() {
    local name="$1"
    shift

    claude mcp remove "$name" -s user 2>/dev/null || true
    claude mcp add "$name" -s user -- "$@"
    codex mcp remove "$name" 2>/dev/null || true
    codex mcp add "$name" -- "$@"
}
add_user_mcp_server lgtmcp "$HOME/bin/lgtmcp"
add_user_mcp_server playwright npx @playwright/mcp@latest --headless
dotfiles_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
python3 "$dotfiles_root/tools/configure_codex.py" "$HOME/.codex/config.toml"

if [[ $os == macos ]]; then
    macos_finish
fi
