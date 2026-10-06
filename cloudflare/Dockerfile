# Copyright © 2026 Michael Shields
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

# The build context is a staged tree (tools/stage_tree.sh DEST), so this builds
# from any directory with: docker build -f DEST/cloudflare/Dockerfile DEST

FROM debian:trixie-slim

# provision.sh installs everything else, through provision/linux-system.sh.
RUN DEBIAN_FRONTEND=noninteractive apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends git sudo ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd -m -u 1000 -U shields \
    && echo 'shields ALL=(ALL) NOPASSWD:ALL' >/etc/sudoers.d/shields \
    && chmod 0440 /etc/sudoers.d/shields

USER shields

# A cache mount creates its missing parent directories as root.
RUN mkdir -p /home/shields/.cache

COPY --chown=1000:1000 . /home/shields/src/github.com/shields/dotfiles
WORKDIR /home/shields/src/github.com/shields/dotfiles

ARG MODULES=dev
ARG TARGETARCH
# Homebrew's cleanup deletes downloads that another build is still using, so
# each architecture has its own cache and a build holds it exclusively. The
# cleanup after provisioning removes what provision.sh leaves behind: Go's
# build cache, npm's cache in /root from its sudo'd Playwright step, and the
# Codex installation id that every container would otherwise share.
RUN --mount=type=cache,target=/home/shields/.cache/Homebrew,id=homebrew-${TARGETARCH},sharing=locked,uid=1000,gid=1000 \
    DOTFILES_KEEP_BREW_DOWNLOADS=1 ./provision.sh $MODULES \
    && rm -rf /home/shields/.cache/go-build /home/shields/.codex/installation_id \
    && sudo rm -rf /var/lib/apt/lists/* /root/.npm /root/.cache

# The cache mount hides Homebrew's API data, which the oh-my-zsh aws plugin
# (brew --prefix awscli) makes the first interactive shell fetch.
RUN /home/linuxbrew/.linuxbrew/bin/brew --prefix awscli >/dev/null

CMD ["sleep", "infinity"]
