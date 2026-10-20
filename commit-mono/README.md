<!--
Copyright © 2026 Michael Shields

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

# Commit Mono custom build

The four OTFs here are generated at <https://commitmono.com/> ("07 Customize",
then "Download custom for dev") from the settings in `custom-settings.json`,
which the site writes into the zip. `tools/create_nerd_commit_mono.sh` patches
them into the "CommitMonoShields Nerd Font" in `Library/Fonts/`, and
`tests/test_nerd_commit_mono.py` checks the result.

## Double oblique hyphen

The build also adds U+2E17 DOUBLE OBLIQUE HYPHEN (`⸗`) from Noto Sans Mono,
so the character is available in the font itself in Emacs and terminal
emulators such as iTerm2. `tools/add_commit_mono_glyph.py` runs after the Nerd
Fonts patcher. It adds the glyph at weights 350 and 700, matching the regular
and bold faces, and applies Commit Mono's 1:6 shear and horizontal centering
to the italic faces. Both fonts use 1000 units per em and a 600-unit advance.
The existing glyphs, line metrics, and OpenType substitutions are preserved.

`noto-sans-mono/NotoSansMono-DoubleObliqueHyphen.ttf` is a subset of the
Noto Sans Mono 2.014 variable font containing only U+2E17 and `.notdef`.
The source is pinned to
[Google Fonts commit 47d54ca1ebb14fd864695f1d39dc734f39017c33](https://github.com/google/fonts/tree/47d54ca1ebb14fd864695f1d39dc734f39017c33/ofl/notosansmono).
The upstream file's SHA-256 is
`2cb2adb378a8f574213e23df697050b83c54c27df465a2015552740b2769a081`.
To regenerate the subset from the repository root:

```sh
curl --fail --location \
    'https://raw.githubusercontent.com/google/fonts/47d54ca1ebb14fd864695f1d39dc734f39017c33/ofl/notosansmono/NotoSansMono%5Bwdth,wght%5D.ttf' \
    --output /tmp/NotoSansMono.ttf
uv run fonttools subset /tmp/NotoSansMono.ttf \
    --unicodes=U+2E17 --name-IDs='*' --name-languages='*' --name-legacy \
    --no-recalc-timestamp --recalc-bounds \
    --output-file=commit-mono/noto-sans-mono/NotoSansMono-DoubleObliqueHyphen.ttf
```

The subset's SHA-256 is
`79a7324d5e446579a82c3dc2b7d189b48ff08485c041f205e7fa05e4e3220d5b`.
Its original notices are retained, and the unmodified upstream license is in
[`noto-sans-mono/OFL.txt`](noto-sans-mono/OFL.txt). The final OTFs retain Commit
Mono's notices and additionally embed the donor's copyright and full OFL
license in their name tables.

## Spacing

`letterSpacing` and `lineHeight` are chosen so that CommitMonoShields at 12pt
gets the same 7 x 14 pt cell that Andale Mono had, in both iTerm2 and Emacs,
with iTerm2's Vertical Spacing left at 1.

- Andale Mono's cell is 0.600 em wide and 1.125 em tall (`hhea` 1858/-446 at
  UPM 2048). Commit Mono's advance is already 0.600 em, so `letterSpacing`
  stays at 0. Its `lineHeight` $L$ sets `hhea` to $900 + 500(L - 1)$ and
  $-(200 + 500(L - 1))$ at UPM 1000, so 1.0 is 1.10 em and 1.05 is 1.15 em;
  the site only offers steps of 0.05.
- iTerm2 3.6 makes the cell $\mathrm{round}(\text{advance})$ wide and
  $\lceil \text{ascent} + \text{descent} \rceil$ tall, and puts the baseline
  $\lfloor \text{descent} + 0.5 \rfloor$ points above the bottom of the cell.
  CoreText reports CFF metrics a hair above exact, so a line height that is a
  whole number of points at the working size rounds up one more (975/-275 at
  12pt is 15.00003 pt and got a 16 pt cell). Emacs (mac-ct) uses
  $\lfloor \text{ascent} + 0.5 \rfloor + \lfloor \text{descent} + 0.5 \rfloor$
  for the line and $\mathrm{round}(\text{advance})$ for the character width.
- At 12pt, lineHeight 1.0 gives 14 in iTerm2 but with the baseline 2 pt up
  instead of Andale Mono's 3, and $11 + 2 = 13$ in Emacs; 1.05 gives 14 in
  both with the baseline where Andale Mono's was (iTerm2
  $\lfloor 2.7 + 0.5 \rfloor = 3$ from the bottom, Emacs
  $\lfloor 11.1 + 0.5 \rfloor = 11$ from the top); 1.1 and up give 15 or
  more. Hence 1.05. It matches Andale Mono from 9pt to 13pt; an off-grid
  1.02 would match at every size from 8pt to 20pt but cannot be entered
  through the site's controls or its JSON import.
- Emacs needs `:weight semi-light` for the weight-350 build, and the default
  face must name it with `:font`, not `:family` (see
  `.emacs.d/lisp/init-display.el`).
- Ghostty 1.3 gives this build the same 7 x 14 pt cell at 12pt with no
  `adjust-cell-width` (measured; Andale Mono got 7 x 13.5 there, and the
  old -5% made either font 6.5 pt wide).
