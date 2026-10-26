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

# Every zsh reads this file, including the non-interactive ones that
# `ssh host command` and `limactl shell vm command` start. It must stay free
# of forks, output and secrets: tokens exported here would reach every
# subprocess, undoing the credential scrubbing the agents do.
#
# macOS gets its PATH from /etc/zprofile and .zshrc, so only Linux's PATH is
# touched.
if [[ $OSTYPE == linux* ]]; then
    typeset -U path
    # Directories already on PATH keep their place, so entries that the parent
    # put ahead of them, such as a venv's bin, stay ahead.
    () {
        local -a front=(~/bin ~/.local/bin /home/linuxbrew/.linuxbrew/{bin,sbin}(N))
        path=(${front:|path} $path ~/go/bin(N))
    }
    if [[ -f /etc/dotfiles-github-gateway ]]; then
        export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
        export NODE_EXTRA_CA_CERTS=/etc/ssl/certs/ca-certificates.crt
        export REQUESTS_CA_BUNDLE=/etc/ssl/certs/ca-certificates.crt
    fi
fi

# Claude Code's sandbox on macOS (Anthropic's sandbox-runtime) blocks trustd,
# the service Go asks to verify certificates on macOS, so gh, crane and go fail
# with "x509: OSStatus -26276". The sandbox's only marker is SANDBOX_RUNTIME=1;
# CLAUDECODE=1 is also required because that name is generic. Since Go 1.27, a
# set SSL_CERT_FILE replaces trustd, and the GODEBUG setting (which Go plans to
# remove in 1.31) extends that to programs whose go.mod names an older Go, such
# as crane. Programs built with an older Go toolchain still fail.
#
# Homebrew's ca-certificates bundle stays current with brew upgrades, unlike
# /etc/ssl/cert.pem, a 2021 OpenBSD bundle that lacks many current roots. Roots
# trusted only in the Keychain, such as a local development server's CA, are in
# neither. A CA file that the sandbox sets for its own TLS-terminating proxy is
# kept.
#
# Opening trustd to the sandbox (enableWeakerNetworkIsolation) would let any
# sandboxed command reach the network around the domain allowlist, because
# trustd fetches the URLs named in the certificates it checks.
#
# Linux needs none of this: Go reads root certificates from files there.
if [[ $OSTYPE == darwin* && ${CLAUDECODE-} == 1 && ${SANDBOX_RUNTIME-} == 1 ]]; then
    export SSL_CERT_FILE=${SSL_CERT_FILE:-/opt/homebrew/etc/ca-certificates/cert.pem}
    if [[ ,${GODEBUG-}, != *,x509sslcertoverrideplatform=* ]]; then
        export GODEBUG=${GODEBUG:+$GODEBUG,}x509sslcertoverrideplatform=1
    fi
fi

# uv links packages from its cache into each venv, which only works within one
# volume, and the checkouts are on /Volumes/Data when this Mac has it. A venv on
# another volume copies its packages and uv warns. Spotlight skips a .noindex
# directory.
if [[ -d /Volumes/Data ]]; then
    export UV_CACHE_DIR=${UV_CACHE_DIR:-/Volumes/Data/uv-cache.noindex}
fi
