#!/usr/bin/env bash

# Install what `make lint build test` needs on a GitHub-hosted ubuntu-26.04
# runner, which has none of the Homebrew toolset the Mac and the Linux image are
# provisioned with.
#
# It changes the machine it runs on (sudo apt-get, Homebrew, ~/.local), so it
# refuses to run anywhere but a runner. Every test must run: tests skip when
# codex, cf, zsh-defer or zsh is missing, and DOTFILES_FAIL_ON_SKIP turns any
# skip into a failure.
#
# cf, which npm installs, is pinned and bumped by hand:
# tests/test_codex_policy.py checks the word lists of the cf policy against
# every command in the installed package, so a release that adds a command the
# lists miss would turn every run red until they are updated. Everything else
# floats: the Homebrew formulae and cask, zsh-defer's head, and basedpyright
# from PyPI. shfmt must be 3.14 or newer for `make lint`.

set -euo pipefail

cf_version=1.0.0-rc.5

brew_prefix=/home/linuxbrew/.linuxbrew
local_prefix=$HOME/.local

die() {
    printf 'install_ci_tools: %s\n' "$*" >&2
    exit 1
}

[[ ${GITHUB_ACTIONS:-} == true ]] || die "not a GitHub Actions runner; refusing to change this machine"
[[ -n ${GITHUB_PATH:-} ]] || die "GITHUB_PATH is not set"
[[ $(uname -s) == Linux && $(uname -m) == x86_64 ]] || die "only x86_64 Linux runners are supported"

retry() {
    local attempt
    for attempt in 1 2 3; do
        "$@" && return 0
        if [[ $attempt -lt 3 ]]; then
            sleep $((attempt * 10))
        fi
    done
    die "failed after 3 attempts: $*"
}

# bsdtar is what tests/test_stage_tree.py runs an extra variant against when it
# is on PATH. Without it the variant is dropped, not skipped, so the skip gate
# cannot see it. tests/test_whitespace.py needs a full vim, not vim-tiny.
retry sudo apt-get update
retry sudo env DEBIAN_FRONTEND=noninteractive apt-get install --yes --no-install-recommends \
    bubblewrap libarchive-tools tmux vim zsh

# The image makes /usr/share world-writable, so compaudit rejects the zsh
# directories that other packages created there and compinit aborts.
sudo chmod -R go-w /usr/share/zsh
zsh -fc 'autoload -Uz compaudit; compaudit' || die "zsh completion directories are insecure"

# Homebrew is on the image but not on PATH. Emacs 31, Node 26 and shfmt 3.14 are
# newer than Ubuntu's or the image's, python@3.15 is where .codex/hooks.json
# looks for Python, and gitleaks must have `gitleaks dir`.
export HOMEBREW_NO_AUTO_UPDATE=1 HOMEBREW_NO_ANALYTICS=1 HOMEBREW_NO_ENV_HINTS=1
retry "$brew_prefix/bin/brew" install chezmoi emacs gitleaks node python@3.15 shfmt
retry "$brew_prefix/bin/brew" install --cask codex
export PATH="$brew_prefix/bin:$local_prefix/bin:$PATH"
printf '%s\n' "$brew_prefix/bin" "$local_prefix/bin" >>"$GITHUB_PATH"

retry npm install --global --ignore-scripts "cf@$cf_version"
retry uv tool install basedpyright

clone_zsh_defer() {
    rm -rf "$local_prefix/share/zsh-defer"
    git clone --quiet --depth 1 https://github.com/romkatv/zsh-defer "$local_prefix/share/zsh-defer"
}
retry clone_zsh_defer

# Ubuntu 24.04 confines unprivileged user namespaces with AppArmor, which
# bubblewrap, and so Codex's sandbox, needs.
if ! bwrap --unshare-user --ro-bind / / true 2>/dev/null; then
    sudo sysctl --write kernel.apparmor_restrict_unprivileged_userns=0
    bwrap --unshare-user --ro-bind / / true || die "bubblewrap cannot create a user namespace"
fi

codex --version
DO_NOT_TRACK=1 cf --version
# Tests drop their bsdtar variant when it is missing, which the skip gate
# cannot see.
bsdtar --version
[[ -f $local_prefix/share/zsh-defer/zsh-defer.plugin.zsh ]] || die "zsh-defer is missing"
