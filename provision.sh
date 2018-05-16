#!/bin/bash

set -e

(cd $(dirname "$0") && tar cf - $(find . -type f | grep -v -e '^\./\.git/' -e '^\./[^.]')) | tar xvf -
