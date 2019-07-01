#!/bin/bash

set -e

tar cf - $(find . -type f | grep -v -e '^\./\.git/' -e '^\./[^.]') \
    | (cd "$HOME" && tar xvf -)
