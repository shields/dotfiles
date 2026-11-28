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

unset CDPATH
cd "$(dirname "$0")"
dotfiles_root=$PWD
user=$(id -un)

uname_s=$(uname -s)
case $uname_s in
Darwin) os=macos ;;
Linux) os=linux ;;
*)
    echo "provision.sh: unsupported OS $uname_s; only macOS and Linux are supported" >&2
    exit 1
    ;;
esac

if [[ $os == linux && $(id -u) -eq 0 ]]; then
    echo "provision.sh: run as a regular user with sudo access; Homebrew refuses to run as root" >&2
    exit 1
fi

source provision/modules.sh
source provision/cli-tools.sh
if [[ $os == macos ]]; then
    source provision/macos.sh
fi

# Homebrew's prefix is not chosen until later, so look in each place it can be.
have_brew=0
brew_found=
for brew_candidate in /opt/homebrew/bin/brew /usr/local/bin/brew /home/linuxbrew/.linuxbrew/bin/brew; do
    if [[ -x $brew_candidate ]]; then
        have_brew=1
        brew_found=$brew_candidate
        break
    fi
done
selection_file=$HOME/.config/dotfiles/brew-modules
modules_status=0
modules_resolve "$os" "$dotfiles_root/brew" "$selection_file" "$have_brew" "$@" || modules_status=$?
if [[ $modules_status -eq 3 && $have_brew -eq 1 ]]; then
    echo "brew bundle cleanup would uninstall:" >&2
    # With stdin on a terminal, brew asks whether to proceed and uninstalls on y;
    # reading /dev/null makes this only a preview.
    HOMEBREW_DOTFILES_BREW_MODULES=${modules_selection:-none} "$brew_found" bundle cleanup --file="$dotfiles_root/Brewfile" >&2 </dev/null || true
fi
if [[ $modules_status -ne 0 ]]; then
    exit "$modules_status"
fi
modules_persist "$selection_file" "$modules_selection"
# Brew drops empty HOMEBREW_* variables, so an empty selection is written as none.
export HOMEBREW_DOTFILES_BREW_MODULES=${modules_selection:-none}

# Copy these files.
copy_paths=(
    '.*' bin ':(exclude).github' ':(exclude).chezmoi*'
    ':(exclude).config/bat/config'
    ':(exclude).config/git/ignore'
    ':(exclude).config/starship.toml'
    ':(exclude).config/yt-dlp/config'
    ':(exclude).gitconfig'
    ':(exclude).inputrc'
    ':(exclude).ripgreprc'
    ':(exclude).tmux.conf'
    ':(exclude).vimrc'
)
case $os in
macos)
    copy_paths+=(
        Library
    )
    ;;
linux)
    copy_paths+=(
        ':(exclude).CFUserTextEncoding'
        ':(exclude).iTerm2/com.googlecode.iterm2.plist'
        ':(exclude).config/karabiner'
        ':(exclude).config/ghostty'
        ':(exclude).config/alacritty'
        ':(exclude).config/emacs-plus'
        ':(exclude).gnupg/gpg-agent.conf'
        ':(exclude)bin/chrome-tabs-to-markdown'
        ':(exclude)bin/clean_downloads.py'
        ':(exclude).agents/skills/transcribe'
        ':(exclude).claude/skills/transcribe'
    )
    ;;
esac
git ls-files -z -- "${copy_paths[@]}" | tar --null -cf - -T - | (cd "$HOME" && tar xvf -)

if [[ $os == linux && -f /etc/cog-gateway ]]; then
    rm -f "$HOME/.codex/rules/no-push.rules"
fi

mkdir -p "$HOME/.codex"
{
    cat .agents/AGENTS.md
    printf '\n'
    cat .codex/instructions.md
} >"$HOME/.codex/AGENTS.md"

if [[ $os == linux ]]; then
    sudo "$dotfiles_root/provision/linux-system.sh" "$user"
fi

if [[ $os == macos ]]; then
    macos_emudhra_workaround
fi

# Install Homebrew (which will take tens of minutes).
if [[ $os == linux ]]; then
    HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"
    if [[ ! -x $HOMEBREW_PREFIX/bin/brew ]]; then
        install_script=$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)
        NONINTERACTIVE=1 /bin/bash -c "$install_script"
    fi
else
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

# brew bundle finds cargo only on the PATH brew was started with. Without it,
# the cargo entries install the rust formula, which cleanup then removes.
# Homebrew asks before it installs dependencies, which rustup has on Linux.
# cargo_install_global needs node, which brew bundle has not yet installed on a
# new machine.
if modules_has "$modules_selection" dev; then
    brew install --yes rustup node
    rustup_prefix=$(brew --prefix rustup)
    export PATH="$rustup_prefix/bin:$PATH"
    rustup update stable
    rustup default stable >/dev/null
fi

cargo_crates=$(brew bundle list --cargo)
for cargo_crate in $cargo_crates; do
    cargo_install_global "$cargo_crate"
done

# Homebrew bundle sync. Edit brew/*.Brewfile to change what is installed, or
# use `brew bundle add --file=brew/<module>.Brewfile`.
# Bundle's --upgrade-formulae keeps casks in the Chrome-filtered step below:
# https://docs.brew.sh/Manpage#bundle-subcommand
# Upgrade selection checks both full and short formula names.
brew_formulae=$(brew bundle list --formula | awk -F/ '{print $0; if (NF > 1) print $NF}' | paste -sd, -)
brew bundle --force --no-upgrade --upgrade-formulae="$brew_formulae" | (grep -v '^Using ' || true)
brew bundle cleanup --force
# Suppress upgrade of Chrome since it doesn't like to be upgraded while running.
brew outdated --greedy-auto-updates --cask --quiet | sed '/^google-chrome$/d' | xargs -r brew upgrade --cask --yes
brew autoremove

npm_packages=$(brew bundle list --npm)
for npm_package in $npm_packages; do
    npm_install_global "$npm_package"
done
go_packages=$(brew bundle list --go)
for go_package in $go_packages; do
    go_install_global "$go_package"
done

# Only .zshrc exports DO_NOT_TRACK, and cf reports every run without it.
if modules_has "$modules_selection" cloud; then
    DO_NOT_TRACK=1 completion_install cf cf complete zsh
else
    completion_remove cf
fi
# cargo installs into CARGO_HOME, which this script's PATH need not include.
if modules_has "$modules_selection" dev; then
    completion_install ghist "${CARGO_HOME:-$HOME/.cargo}/bin/ghist" --completions zsh
else
    completion_remove ghist
fi

if ! chezmoi --source "$dotfiles_root" --less-interactive --error-on-conflict apply; then
    echo "provision.sh: chezmoi apply failed; run 'make diff' and resolve any conflicts as described in README.md" >&2
    exit 1
fi

# The image build mounts Homebrew's download cache to keep it for the next
# build, which --prune=all would empty.
if [[ -n ${DOTFILES_KEEP_BREW_DOWNLOADS-} ]]; then
    brew cleanup
else
    brew cleanup --prune=all
fi

# The same cache that .zshenv selects, for a run started outside zsh.
if [[ $HOME/src -ef /Volumes/Data/src ]]; then
    export UV_CACHE_DIR=${UV_CACHE_DIR:-/Volumes/Data/uv-cache.noindex}
fi
uv cache prune

go clean -modcache

if [[ $os == macos ]]; then
    # On Linux, /var/run/docker.sock can exist without the user being able to
    # use it, which bin/docker-prune's socket check cannot tell.
    bin/docker-prune
    macos_xcode
    macos_login_shell
    macos_emudhra_workaround
    macos_claude_model_pricing
fi

# Plugins!
export PIP_DISABLE_PIP_VERSION_CHECK=1
if modules_has "$modules_selection" data; then
    llm install --upgrade llm-{gemini,anthropic,perplexity,cmd,openai-plugin} | (grep -v '^Requirement already satisfied:' || true)
fi
if modules_has "$modules_selection" cloud; then
    gcloud --quiet components update
fi

if [[ $os == macos ]]; then
    macos_defaults
fi

# Emacs setup
emacs --batch --script .emacs.d/provision.el
if [[ $os == macos ]]; then
    macos_emacs_app
fi

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

# Go setup. Tools (goimports, etc.) are installed to GOPATH's bin directory via
# the Brewfile's `go` entries; only Go's own settings belong here.
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

if [[ $os == macos ]]; then
    add_user_mcp_server playwright npx @playwright/mcp@latest --headless
else
    playwright_version=$(npm view @playwright/mcp version)
    add_user_mcp_server playwright npx "@playwright/mcp@$playwright_version" --headless --browser chromium
    npx -y -p "@playwright/mcp@$playwright_version" playwright install chromium
    # The browser's system libraries come from apt, so this needs root, and a
    # second password prompt where sudo asks for one. root's PATH leaves out the
    # user's own bin directories and puts the user-owned Homebrew prefix last, so
    # only node and npx come from it. sudo drops DEBIAN_FRONTEND, so env sets it
    # for the apt-get that Playwright runs.
    sudo -v
    sudo env DEBIAN_FRONTEND=noninteractive PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$HOMEBREW_PREFIX/bin" npx -y -p "@playwright/mcp@$playwright_version" playwright install-deps chromium
fi
uv run --project "$dotfiles_root" --no-dev python "$dotfiles_root/tools/configure_codex.py" "$HOME/.codex/config.toml"

if [[ $os == linux ]]; then
    # Claude Code's sandbox skips a write grant for a path that does not exist,
    # so cf's login directory must exist before the first login.
    if modules_has "$modules_selection" cloud; then
        mkdir -p "$HOME/.config/cloudflare/config"
    fi

    mkdir -p "$HOME/.config/git"
    for credential_url in https://github.com https://gist.github.com; do
        # A gateway guest keeps cog's helper for github.com in ~/.config/git/config.
        if [[ $credential_url == https://github.com && -f /etc/cog-gateway ]]; then
            continue
        fi
        git config --file "$HOME/.config/git/config" "credential.$credential_url.helper" '!gh auth git-credential'
    done

    # Without hasCompletedOnboarding, Claude Code ignores CLAUDE_CODE_OAUTH_TOKEN.
    # A repository is trusted only by its own entry, not a parent directory's,
    # and `claude -p` ignores its permissions.allow until it is.
    claude_json=$HOME/.claude.json
    if [[ ! -s $claude_json ]]; then
        (umask 077 && printf '{}\n' >"$claude_json")
    fi
    claude_json_new=$(mktemp "$claude_json.XXXXXX")
    if ! jq --arg root "$dotfiles_root" \
        '.hasCompletedOnboarding = true | .projects[$root].hasTrustDialogAccepted = true' \
        "$claude_json" >"$claude_json_new" || [[ ! -s $claude_json_new ]]; then
        rm -f "$claude_json_new"
        echo "provision.sh: cannot update $claude_json" >&2
        exit 1
    fi
    mv "$claude_json_new" "$claude_json"
fi

if [[ $os == macos ]]; then
    macos_finish
fi
