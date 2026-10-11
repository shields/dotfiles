#!/bin/bash

set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)
secret_dir=$HOME/.config/secrets/vm-access
token_file=$secret_dir/cloudflare-admin-token
install -d -m 0700 "$secret_dir"
if [[ ! -e $token_file ]]; then
    umask 077
    openssl rand -hex 32 >"$token_file"
fi
[[ $(stat -f %Lp "$token_file" 2>/dev/null || stat -c %a "$token_file") == 600 ]] || {
    printf '%s\n' 'Cloudflare admin token must have mode 0600' >&2
    exit 1
}
[[ $(wc -c <"$token_file") -eq 65 ]] || {
    printf '%s\n' 'Cloudflare admin token is invalid' >&2
    exit 1
}

stage=$(mktemp -d "${TMPDIR:-/tmp}/dotfiles-cloudflare.XXXXXX")
trap 'rm -rf "$stage"' EXIT
"$root/tools/stage_tree.sh" "$stage/tree"
cd "$stage/tree"
public_key=$(cat "$HOME/.ssh/id_ed25519.pub")
[[ $public_key == ssh-ed25519\ * ]] || {
    printf '%s\n' 'Cloudflare SSH requires ~/.ssh/id_ed25519.pub' >&2
    exit 1
}
jq --arg key "$public_key" \
    '.containers[0].authorized_keys = [{name: "owner", public_key: $key}]' \
    cloudflare/wrangler.jsonc >"$stage/wrangler.jsonc"
mv "$stage/wrangler.jsonc" cloudflare/wrangler.jsonc
bun install --frozen-lockfile
umask 077
{
    printf 'ADMIN_TOKEN='
    cat "$token_file"
} >"$stage/admin.env"
bunx wrangler deploy --config cloudflare/wrangler.jsonc --secrets-file "$stage/admin.env"
