#!/bin/bash

set -e

tar cf - $(find . -type f | grep -v -e '^\./\.git/' -e '^\./[^.]') \
    | (cd "$HOME" && tar xvf -)

# Is this a Freenome-managed Mac?
if [[ "$(uname)" == Darwin ]] && \
       system_profiler SPConfigurationProfileDataType | grep -qi freenome; then
    git config --global user.email michael.shields@freenome.com
else
    git config --global user.email shields@msrl.com
fi
