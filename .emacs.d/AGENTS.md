<!--
Copyright © 2025-2026 Michael Shields

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# AGENTS.md

## Code Style

- Lexical binding, use-package based, use keymap-set
- Set faces in `shields-theme.el` or a package's `:custom-face`, never with
  `custom-set-faces`: Custom copies faces set that way into the untracked
  `~/.emacs.d/custom.el`, which loads before the init files and is never
  refreshed.

## Conventions

- Do not optimize Emacs startup time - use emacsclient
- Prefer tree-sitter modes when available
