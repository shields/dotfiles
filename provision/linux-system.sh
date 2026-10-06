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

set -euo pipefail

if [[ $(id -u) -ne 0 ]]; then
    echo "linux-system.sh: must run as root; use: sudo $0 USER" >&2
    exit 1
fi
if [[ $# -ne 1 ]]; then
    echo "usage: sudo $0 USER" >&2
    exit 2
fi
user=$1
if [[ $user == root ]]; then
    echo "linux-system.sh: USER must be the regular account, not root; Homebrew refuses to run as root" >&2
    exit 1
fi
if ! group=$(id -gn "$user" 2>/dev/null); then
    echo "linux-system.sh: no such user: $user" >&2
    exit 1
fi
if ! command -v apt-get >/dev/null; then
    echo "linux-system.sh: apt-get not found; Debian is required" >&2
    exit 1
fi

export DEBIAN_FRONTEND=noninteractive

# apt-get update takes the lists lock without waiting, and DPkg::Lock::Timeout
# covers only the dpkg locks, so a concurrent apt run (cloud-init, apt-daily) is
# waited out here. LC_ALL=C keeps the lock error matchable.
deadline=$((SECONDS + 600))
while :; do
    status=0
    output=$(LC_ALL=C apt-get -o DPkg::Lock::Timeout=600 update 2>&1) || status=$?
    if [[ $status -eq 0 ]]; then
        printf '%s\n' "$output"
        break
    fi
    if [[ $output != *"Could not get lock"* || $SECONDS -ge $deadline ]]; then
        printf '%s\n' "$output" >&2
        exit "$status"
    fi
    echo "linux-system.sh: waiting for another apt process to release its lock" >&2
    sleep 10
done

apt-get -o DPkg::Lock::Timeout=600 install -y --no-install-recommends \
    build-essential procps curl file git zsh vim locales ca-certificates \
    bubblewrap socat tmux ncurses-term unzip jq openssh-client

locales=$(locale -a)
if ! grep -qix 'en_US\.utf-\?8' <<<"$locales"; then
    sed -i 's/^# *\(en_US\.UTF-8 UTF-8\)/\1/' /etc/locale.gen
    if ! grep -qx 'en_US\.UTF-8 UTF-8' /etc/locale.gen; then
        echo 'en_US.UTF-8 UTF-8' >>/etc/locale.gen
    fi
    locale-gen
fi

# Debian's zsh is the login shell so that a broken Linuxbrew cannot lock out
# logins. It must start without Oh My Zsh, which is installed after this runs.
login_shell=$(getent passwd "$user" | cut -d: -f7)
if [[ $login_shell != /usr/bin/zsh ]]; then
    chsh -s /usr/bin/zsh "$user"
fi

# Homebrew's installer needs no sudo when its prefix already belongs to the user.
install -d -m 755 /home/linuxbrew
install -d -m 755 -o "$user" -g "$group" /home/linuxbrew/.linuxbrew
