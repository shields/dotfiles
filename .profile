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
test -x /usr/bin/less && PAGER=less
export EDITOR VISUAL PAGER

# less. 0.382 ≈ 1/φ. 16.226 matches the macOS findHighlightColor.
LESS='-eFiMX -j.382 --incsearch --mouse --use-color --color=PWc$ --color=S16.226$'
export LESS

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
