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

@../.agents/AGENTS.md

# Claude Code instructions

## Git and commits

- After a code change, run `/code-review --fix` before review/commit: `max` for
  changes to logic or behavior, `high` for small or mechanical ones (constants,
  renames, test-only or documentation edits). Then, after applying its fixes or
  any later small or straightforward change to the reviewed code, look over just
  what changed; don't start another full sweep.

## Web access

- For online PDFs, download with `curl` and open with Read.
