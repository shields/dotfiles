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

# Every zsh reads this file, including the non-interactive ones that
# `ssh host command` and `limactl shell vm command` start. It must stay free
# of forks, output and secrets: tokens exported here would reach every
# subprocess, undoing the credential scrubbing the agents do.
#
# macOS gets its PATH from /etc/zprofile and .zshrc, so only Linux is touched.
if [[ $OSTYPE == linux* ]]; then
    typeset -U path
    path=(~/bin ~/.local/bin /home/linuxbrew/.linuxbrew/{bin,sbin}(N) $path ~/go/bin(N))
fi
