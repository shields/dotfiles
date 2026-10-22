#!/usr/bin/env bash

# Install what `make lint build test` needs on a GitHub-hosted ubuntu-24.04
# runner, which has none of the Homebrew toolset the Mac and the Linux image are
# provisioned with.
#
# It changes the machine it runs on (sudo apt-get, Homebrew, ~/.local), so it
# refuses to run anywhere but a runner. Every test must run: tests skip when
# codex, limactl, zsh-defer or zsh is missing, and DOTFILES_FAIL_ON_SKIP turns
# any skip into a failure.
#
# Lima is not from Homebrew because its formula pulls in qemu; it is pinned with
# a checksum and bumped by hand. Everything else floats: the Homebrew formulae
# and cask, zsh-defer's head, and basedpyright from PyPI. shfmt must be 3.14 or
# newer for `make lint`.

set -euo pipefail

lima_version=2.2.1
lima_sha256=b391ac7fcac2b2a5a1628107756463188ded4d18d3104a40a66a8bb6fec01bcb

brew_prefix=/home/linuxbrew/.linuxbrew
local_prefix=$HOME/.local

die() {
    printf 'install_ci_tools: %s\n' "$*" >&2
    exit 1
}

[[ ${GITHUB_ACTIONS:-} == true ]] || die "not a GitHub Actions runner; refusing to change this machine"
[[ -n ${GITHUB_PATH:-} ]] || die "GITHUB_PATH is not set"
[[ $(uname -s) == Linux && $(uname -m) == x86_64 ]] || die "only x86_64 Linux runners are supported"

work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT

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

download() {
    local url=$1 sha256=$2 dest=$3
    curl --fail --silent --show-error --location --retry 3 --retry-all-errors \
        --connect-timeout 20 --max-time 600 --speed-limit 102400 --speed-time 30 \
        --output "$dest" "$url"
    printf '%s  %s\n' "$sha256" "$dest" | sha256sum --check --strict --status ||
        die "checksum mismatch for $url"
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
# looks for Python, and gitleaks must have `gitleaks dir`. python@3.13 is what
# tests/test_github_app_token.py parses the token tool under when python3.13 is
# on PATH, again without a skip when it is not.
export HOMEBREW_NO_AUTO_UPDATE=1 HOMEBREW_NO_ANALYTICS=1 HOMEBREW_NO_ENV_HINTS=1
retry "$brew_prefix/bin/brew" install emacs gitleaks node python@3.13 python@3.15 shfmt
retry "$brew_prefix/bin/brew" install --cask codex
export PATH="$brew_prefix/bin:$local_prefix/bin:$PATH"
printf '%s\n' "$brew_prefix/bin" "$local_prefix/bin" >>"$GITHUB_PATH"

retry uv tool install basedpyright

mkdir -p "$local_prefix/bin"
download "https://github.com/lima-vm/lima/releases/download/v$lima_version/lima-$lima_version-Linux-x86_64.tar.gz" \
    "$lima_sha256" "$work_dir/lima.tar.gz"
tar -xzf "$work_dir/lima.tar.gz" -C "$local_prefix"

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
limactl --version
# Tests drop their bsdtar and python3.13 variants when these are missing, which
# the skip gate cannot see.
bsdtar --version
python3.13 --version
[[ -f $local_prefix/share/zsh-defer/zsh-defer.plugin.zsh ]] || die "zsh-defer is missing"
