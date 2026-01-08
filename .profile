#
# ~/.profile
# Michael Shields <shields@msrl.com>
#

umask 022

# Terminal and locale setup.
if [ -t 0 ]; then
    stty erase '^?'
    stty cs8
    stty -ixon
fi

if [ -z "$LANG" ]; then
    LANG=en_US.UTF-8
    export LANG
fi

if [ "$TERM" = linux ]; then
    tty | fgrep -q /dev/vc && unicode_start
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
