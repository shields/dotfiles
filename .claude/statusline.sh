#!/bin/bash

set -euo pipefail

(git rev-parse --short HEAD; starship module git_status) | tr -d '\n'
