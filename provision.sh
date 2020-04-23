#!/bin/sh

set -e

tar cf - $(find . -type f | grep -v -e '^\./\.git/' -e '^\./[^.]') \
    | (cd "$HOME" && tar xvf -)

# The interesting part of
# https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh
if [[ ! -d "$HOME/.oh-my-zsh" ]]; then
    git clone --depth=1 https://github.com/ohmyzsh/ohmyzsh "$HOME/.oh-my-zsh"
fi

# Is this a Freenome-managed Mac?
if [[ "$(uname)" == Darwin ]] && \
       system_profiler SPConfigurationProfileDataType | grep -qi freenome; then
    git config --global user.email michael.shields@freenome.com
else
    git config --global user.email shields@msrl.com
fi
