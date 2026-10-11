#!/bin/bash

set -euo pipefail

ca=/etc/cloudflare/certs/cloudflare-containers-ca.crt
[[ -s $ca ]] || {
    printf '%s\n' 'Cloudflare outbound CA is missing' >&2
    exit 1
}
sudo install -m 0644 "$ca" /usr/local/share/ca-certificates/cloudflare-containers-ca.crt
sudo update-ca-certificates >/dev/null
exec sleep infinity
