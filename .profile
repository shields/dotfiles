# Copyright © 2001-2003, 2006, 2009, 2018-2020, 2022, 2024-2026 Michael Shields
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

# shellcheck shell=sh
umask 022

# The skeleton .profile that Debian installs, which this file replaces, is what
# puts ~/bin and ~/.local/bin on a bash login shell's PATH; Linuxbrew needs
# adding too. This is the PATH that .zshenv gives zsh. Bash and zsh set OSTYPE;
# a shell that does not, such as dash, skips this.
# shellcheck disable=SC3028
case ${OSTYPE:-} in
linux*)
    for dir in /home/linuxbrew/.linuxbrew/sbin /home/linuxbrew/.linuxbrew/bin; do
        [ -d "$dir" ] || continue
        case ":$PATH:" in
        *":$dir:"*) ;;
        *) PATH="$dir:$PATH" ;;
        esac
    done
    for dir in "$HOME/.local/bin" "$HOME/bin"; do
        case ":$PATH:" in
        *":$dir:"*) ;;
        *) PATH="$dir:$PATH" ;;
        esac
    done
    if [ -d "$HOME/go/bin" ]; then
        case ":$PATH:" in
        *":$HOME/go/bin:"*) ;;
        *) PATH="$PATH:$HOME/go/bin" ;;
        esac
    fi
    unset dir
    ;;
esac

# Terminal and locale setup.
if [ -t 0 ]; then
    stty erase '^?' cs8 -ixon
fi

if [ -z "$LANG" ]; then
    LANG=en_US.UTF-8
    export LANG
fi

if [ "$TERM" = linux ]; then
    tty | grep -F -q /dev/vc && unicode_start
fi

# Set default editor and pager.
EDITOR="vi"
VISUAL="$EDITOR"
PAGER="$HOME/bin/pager"
export EDITOR VISUAL PAGER

# Claude Code.
export CLAUDE_CODE_DISABLE_FEEDBACK_SURVEY=1

# pyenv.
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

# ripgrep.
RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export RIPGREP_CONFIG_PATH

# rsync.
RSYNC_RSH=ssh
export RSYNC_RSH

# vi.
EXINIT=':set ai'
export EXINIT
