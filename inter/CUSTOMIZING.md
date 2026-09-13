# Customizing Inter without forking it

Notes from an investigation in September 2026 into three personal changes to
Inter: a slightly lighter Regular, tighter letter spacing, and some character
variants enabled by default. The conclusion is that none of these justify a
fork. All three can be baked into the released font binaries with a handful of
commands, which is also what the maintainer recommends for feature defaults.
Everything below was checked against the source at commit 353b61b9f, the v4.1
release binaries, and live shaping with HarfBuzz.

Contents

1. Decision
2. State of the project
3. What is inside the variable font
4. Weight
5. Character variants
6. Optical size
7. Letter spacing
8. The recipe
9. Installing on macOS
10. If you fork anyway
11. Pitfalls
12. Sources

## 1. Decision

Do not fork for these three changes. Post-process the released variable fonts
instead:

- Character variants on by default: OpenType Feature Freezer, the tool Inter's
  own FAQ points to. Verified to keep the font variable and to shape and kern
  identically to enabling the features.
- Lighter Regular: shift the variable font's default weight with the fontTools
  instancer and relabel the axis so user weight 400 renders at a lighter
  design coordinate. Verified on the shipped font.
- Tighter spacing: subtract a constant from every advance width. The variable
  font's width deltas are additive, so every instance shifts by the same
  amount. Verified with hb-shape at several weights.

Fork only if you want to change actual letterforms or tune spacing per glyph.
Section 10 covers what that costs.

## 2. State of the project

- Latest release is v4.1 from 2024-11-16. The upstream master branch has had
  no commits since 2024-11-19 (commit 353b61b9f, the same commit this checkout
  is on). version.txt says 4.2 but no 4.2 has been released. A January 2025
  discussion asking about the next release has no reply.
- LICENSE.txt is SIL OFL 1.1 and declares no Reserved Font Name after the
  copyright line. README.md nevertheless states that "Inter" is a Reserved
  Font Name and a trademark of Rasmus Andersson, and the shipped fonts carry a
  trademark string in name ID 7. Rename any derivative.
- The release zip contains Inter.ttc (36 hinted static fonts, families "Inter"
  and "Inter Display"), InterVariable.ttf and InterVariable-Italic.ttf (family
  "Inter Variable", unhinted), web/ (woff2 plus inter.css), and extras/ with
  static OTF, hinted TTF, and hinted woff2. The variable fonts are named
  differently from the statics on purpose so both can be installed together.
- Inter Tight on Google Fonts was a special build for Google Workspace. Its
  source repository was archived on 2024-02-12, it has only a weight axis, and
  it does not receive Inter 4 updates.
- The three forks listed in the README were built different ways: Interalia
  from the Glyphs source with fontmake, Raveo by exporting a reduced source
  directly from Glyphs.app against the maintainer's advice, and Open Runde by
  applying a corner-rounding filter in Glyphs Mini.

## 3. What is inside the variable font

- Axes: wght 100 to 900 with default 400, and opsz 14 to 32 with default 14.
  Italic is a separate file with the same axes.
- Masters: only Thin, Regular, and Black at each optical size, so six per
  file and twelve across roman and italic. Every other weight is linear
  interpolation between adjacent masters.
- Weight remapping: an avar table maps user weights 500, 600, 700, and 800 to
  design coordinates 490, 580, 670, and 780. Weights at or below 400 pass
  through unchanged. STAT, fvar, and CSS all see the round numbers.
- Named instances in the compiled font: nine, Thin through Black, all at
  optical size 14. The source defines a second set at optical size 32, but
  those ship only as the static "Inter Display" fonts.
- Tables relevant to post-processing: HVAR carries advance-width deltas
  relative to the default master, GDEF carries a separate variation store for
  kerning, and there are no TrueType hinting tables at all in the variable
  fonts.

## 4. Weight

Commit b1fb2c8ed (2023-06-05, "10u bolder stems of regular text") made the
Regular master's vertical stems 10 units heavier on the 2048 unit em. The
lowercase l stem went from 170 to 180 and its advance from 486 to 496. The
file misc/2023-06-06-regular-stroke-weight-increase.txt contains only the
value 22.3, and 400 minus 22.3 is 377.7. Measuring the shipped font confirms
that weight 378 reproduces the pre-2023 stems of l and I to within a unit
(other glyphs land within a unit or two; H solves to about 379). Nothing in
the repository documents where 22.3 came from, so treat that match as strong
circumstantial evidence rather than a stated fact.

Stem width of H at optical size 14, measured by instancing InterVariable.ttf
and rounding the outline coordinates to whole units. I has the same stem
width as H at every weight checked:

| wght | H stem (units) |
|---|---|
| 100 | 46 |
| 300 | 142 |
| 350 | 166 |
| 378 | 179 |
| 400 | 190 |
| 500 | 228 |
| 600 | 267 |
| 700 | 305 |
| 900 | 404 |

Options, lightest touch first:

1. On the web, `font-weight: 378` just works because weights below 400 are
   not remapped.
2. For installed fonts, the lighter_regular.py script in section 8 makes user
   weight 400 render at a chosen design weight while leaving 100 and 500
   through 900 exactly as shipped.
3. Fully pinned statics at any weight via `fonttools varLib.instancer`. Note
   that `--update-name-table` only works for weights that have a STAT entry
   (the round hundreds); at other values you rename by hand.

Inter's own build never uses the instancer. The variable font's default
location is tied to a real master in the source, so moving it at the source
level means drawing a new master (section 10).

## 5. Character variants

The v4.1 release has these opt-in features. The unreleased source also adds
cv15 (simplified tabular one) and cv16 (a with tail).

| Tag | Effect |
|---|---|
| cv01 | Alternate one |
| cv02 | Open four |
| cv03 | Open six |
| cv04 | Open nine |
| cv05 | Lower-case l with tail |
| cv06 | Simplified u |
| cv07 | German double-s alternate |
| cv08 | Upper-case I with serifs |
| cv09 | Flat-top three |
| cv10 | Capital G with spur |
| cv11 | Single-storey a |
| cv12 | Compact f |
| cv13 | Compact t |
| cv14 | Capital German double-s alternate |
| ss01 | Open digits (bundles cv02, cv03, cv04, cv09) |
| ss02 | Disambiguation, includes slashed zero |
| ss03 | Round quotes and commas |
| ss04 | Disambiguation without slashed zero |
| ss05 | Circled characters |
| ss06 | Squared characters |
| ss07 | Square punctuation |
| ss08 | Square quotes |

How they are built:

- Every cv and ss rule is a flat single substitution such as
  `sub l by l.ss02;`, one line per glyph including every accented form. There
  are no contextual rules in these features. That is exactly the subset
  OpenType Feature Freezer supports, which is why freezing works cleanly.
- Hand-written rules live in src/features/*.fea. ss01, ss03, ss07, and ss08
  exist only inside the Glyphs source (fontinfo.plist), and ss03, ss07, and
  ss08 are generated by Glyphs from glyph-name suffixes.
- cv05 substitutes the same glyph as ss02's l (l.ss02); there is no separate
  cv05-only glyph.
- The italic font has no cv11 at all, because its a is single-storey by
  design. Asking the freezer for a tag the font lacks is harmless, so the
  same command works for both files.
- Alternate glyphs carry their own kerning groups in the source (l.ss02 has a
  different right group from l, a.1 has different groups from a), and their
  accented forms are separate pre-built composites (lacute.ss02 next to
  lacute). Post-build freezing keeps all of that intact because the compiled
  kerning already covers the alternate glyphs. Making a variant the default
  at the source level would mean re-pointing every kerning group and every
  composite for that letter by hand.

Maintainer position: the FAQ entry "Can I change what OpenType features are
enabled by default?" answers with a link to OpenType Feature Freezer, and the
maintainer declined a request for tabular figures by default in 2022 with the
same pointer.

Verification: after freezing cv05 and cv11 into InterVariable.ttf, hb-shape
produced identical glyph runs and advances to the original font with
`--features=cv05,cv11`, at both 380 and 700 weight. fvar, gvar, and HVAR were
untouched.

## 6. Optical size

The opsz axis is a designed axis with separate Text (14) and Display (32)
masters. Moving toward Display changes several things at once, so it is not a
tracking control. Measured at weight 400:

| Glyph | opsz 14 | opsz 32 |
|---|---|---|
| n advance | 1210 | 1120 |
| o advance | 1228 | 1124 |
| a advance | 1150 | 1060 |
| H advance | 1522 | 1450 |
| x height | 1118 | 1056 |

- Narrowing is larger at Thin (about 11% on n) and smaller at Black (about
  5%). The change is close to linear along the axis: n is 1210, 1180, 1150,
  and 1120 at opsz 14, 20, 26, and 32.
- Kerning is retuned per pair between the two masters, not scaled. About
  three quarters of shared pairs are identical; the rest go both ways.
- Browsers apply opsz automatically with the default
  `font-optical-sizing: auto`, setting it to the font size in CSS pixels, so
  16px text already renders at opsz 16 and headlines from 32px up get the
  Display design. Sizes below 14px clamp to 14. Desktop apps vary; many use
  the default of 14. The release also ships the Display end as a static
  family.
- To pin or shift it for apps that never set it:

```sh
fonttools varLib.instancer -o out.ttf InterVariable.ttf opsz=20        # fixed
fonttools varLib.instancer -o out.ttf InterVariable.ttf opsz=14:20:32  # new default
```

## 7. Letter spacing

There is no tracking feature in the font. Spacing is per-glyph sidebearings
in each master, kerning, the opsz axis, and whatever the application adds.

Inter's "dynamic metrics" page was removed from the website (docs/dynmetrics
now redirects to an archived copy), but the formula is still live in the Lab
page (docs/lab/index.html), where it fills in the default letter-spacing:
tracking in em = a + b·e^(c·size) with a = -0.0223, b = 0.185, c = -0.1745
for the text design. It is a useful calibration for how much to tighten:

| Size (px) | Tracking (em) | Units at 2048 upm |
|---|---|---|
| 12 | 0.000 | 1 |
| 14 | -0.006 | 13 |
| 16 | -0.011 | 22 |
| 20 | -0.017 | 34 |
| 24 | -0.020 | 40 |
| 32 | -0.022 | 44 |

Options:

1. CSS `letter-spacing`. Web only.
2. Subtract N units from every non-zero advance width (tighten_hmtx.py in
   section 8). Equivalent to letter-spacing baked in: outlines do not move,
   kerning and marks are untouched, tabular figures stay equal, and because
   HVAR deltas are additive every weight and optical size shifts by exactly N.
   Verified at weights 400 and 700.
3. Per-weight amounts, as Inter Tight did: its README lists no change at
   Thin, 48 units less at Regular, and 24 less at Black. Not possible on the
   variable font with the simple script; either edit the HVAR deltas or
   generate statics per weight and tighten each by a different N.
4. Pin or shift opsz (section 6), accepting the lower x-height.
5. Inter Tight itself, with the caveats in section 2.
6. Symmetric trimming that also moves outlines left by N/2. No visible gain in
   running text over option 2, and it requires handling composites, marks,
   and hints. Only matters if the first glyph on a line must sit flush.
7. Editing sidebearings in the source: about 3,000 glyph files per master,
   twelve masters, followed by kerning review. The only way to tighten some
   letters more than others.

## 8. The recipe

Tested end to end with fonttools 4.64.0 and 4.65.0, opentype-feature-freezer
1.32.2, and HarfBuzz hb-shape on the v4.1 release, for both the roman and
italic fonts. The feature tags, weight, and tracking amount below are
placeholders; substitute your own.

```sh
uv venv .venv --python 3.14
uv pip install --python .venv/bin/python 'fonttools[woff]==4.65.0' 'opentype-feature-freezer==1.32.2'
curl -LO https://github.com/rsms/inter/releases/download/v4.1/Inter-4.1.zip
unzip -q Inter-4.1.zip -d Inter-4.1

# 1. Variants on by default.
.venv/bin/pyftfeatfreeze -f 'cv05,cv11' Inter-4.1/InterVariable.ttf step1.ttf

# 2. User weight 400 now renders at design weight 378.
.venv/bin/python lighter_regular.py step1.ttf step2.ttf 378

# 3. Tracking: remove 41 units (2% of the em) from every advance.
.venv/bin/python tighten_hmtx.py step2.ttf step3.ttf 41

# 4. New family name, then fix the PostScript names the tool skips.
.venv/bin/python misc/tools/rename.py --family "Inter Custom" -o InterCustom.ttf step3.ttf
.venv/bin/python fix_psnames.py InterCustom.ttf InterVariable InterCustom

# Optional statics for software without variable-font support.
.venv/bin/fonttools varLib.instancer --update-name-table -o InterCustom-Regular.ttf InterCustom.ttf wght=400 opsz=14
```

The static instancer names the result from STAT, which appends the optical
size label: family "Inter Custom Text", PostScript "InterCustom-Text". Run
the rename tool on it afterwards if you want a plain "Inter Custom". The
"Attempting to fix OTLOffsetOverflowError" line it prints is fontTools
repacking a large GPOS table and is not an error.

Repeat for InterVariable-Italic.ttf with different intermediate and output
names (step1i.ttf through step3i.ttf, InterCustom-Italic.ttf), otherwise the
second pass overwrites the roman files.

lighter_regular.py: rebase the default master to the chosen weight, then
relabel the axis so that weight reads as 400 and rebuild avar so 500 through
800 still land on the shipped design coordinates.

```python
import sys
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

src, dst, new_default = sys.argv[1], sys.argv[2], float(sys.argv[3])
f = instancer.instantiateVariableFont(TTFont(src), {"wght": (100, new_default, 900)}, inplace=False)
axis = next(a for a in f["fvar"].axes if a.axisTag == "wght")
axis.defaultValue = 400.0
pairs = [(100, 100), (400, new_default), (500, 490), (600, 580), (700, 670), (800, 780), (900, 900)]
def nu(u): return (u - 400) / (500 if u > 400 else 300)
def nd(d): return (d - new_default) / ((900 - new_default) if d > new_default else (new_default - 100))
f["avar"].segments["wght"] = {nu(u): nd(d) for u, d in pairs}
f["OS/2"].usWeightClass = 400
f.save(dst)
```

tighten_hmtx.py: uniform tracking.

```python
import sys
from fontTools.ttLib import TTFont

src, dst, n = sys.argv[1], sys.argv[2], int(sys.argv[3])
f = TTFont(src)
hmtx = f["hmtx"]
for g, (adv, lsb) in hmtx.metrics.items():
    if g != ".notdef":
        hmtx.metrics[g] = (max(adv - n, 0), lsb)
f["hhea"].advanceWidthMax = max(a for a, _ in hmtx.metrics.values())
f.save(dst)
```

The clamp matters: in the italic, two combining marks come out of the
weight step with an advance of 1 unit, and an unclamped subtraction produces
a negative width that fontTools refuses to write.

fix_psnames.py: misc/tools/rename.py rewrites the family names but leaves
"InterVariable" in name ID 25 (the prefix for generated PostScript names) and
in every named instance's own PostScript name record (name IDs 259 and up,
referenced from fvar). This rewrites all of them.

```python
import sys
from fontTools.ttLib import TTFont

path, old, new = sys.argv[1], sys.argv[2], sys.argv[3]
f = TTFont(path)
name = f["name"]
ids = {25} | {i.postscriptNameID for i in f["fvar"].instances if i.postscriptNameID != 0xFFFF}
for rec in name.names:
    if rec.nameID in ids:
        rec.string = rec.toUnicode().replace(old, new)
f.save(path)
```

Checks worth running afterwards:

```sh
# shaping and kerning: compare with the original with features enabled
hb-shape --features=cv05,cv11 --variations=wght=378 Inter-4.1/InterVariable.ttf "Wall flag"
hb-shape --variations=wght=400 InterCustom.ttf "Wall flag"    # same glyphs, advances minus 41
# axes, instances, names
.venv/bin/ttx -t fvar -t name -o - InterCustom.ttf | less
```

## 9. Installing on macOS

- Give the custom fonts their own family and PostScript names before
  installing. Font Book checks new fonts for duplicates by their name
  records, the PostScript name included, and applications resolve two fonts
  with the same names unpredictably.
- Run fix_psnames.py as in the recipe. Each named instance in fvar points at
  its own PostScript name record (InterVariable-Bold and so on), and name ID
  25 is the prefix used for PostScript names at arbitrary coordinates. The
  repo's rename tool rewrites neither, so without the fix a renamed font
  still claims the same instance PostScript names as stock Inter Variable.
- Install only the custom variable fonts, roman and italic. Do not add custom
  statics under the same family name. Generate statics only for a specific
  application that cannot use variable fonts, and give them a distinct name.
- Keep stock Inter installed. Anything that asks for Inter by name, such as
  web pages with local font lookups, other people's documents, or
  applications that hardcode the family, should still get the original.
  Freezing is also one-way: the custom font cannot show the plain l again.

## 10. If you fork anyway

Building works and does not need Glyphs.app:

- The pipeline is glyphspkg (package to .glyphs), fontmake to UFO and
  designspace, misc/tools/postprocess-designspace.py,
  misc/tools/gen-var-designspace.py, fontmake to variable, then
  misc/tools/bake-vf.py for STAT and names. The Makefile also symlinks
  src/features into the UFO directory; without it fontmake fails to find the
  included feature files.
- The pinned toolchain (fontmake 3.9.0, fonttools 4.51.0, glyphsLib 6.7.1,
  ufo2ft 3.2.3, skia-pathops 0.8.0.post1) installs cleanly with uv on Python
  3.11. It does not install on Python 3.14 because skia-pathops 0.8 has no
  wheel and its source build fails. Installing a subset without the pinned
  setuptools 70 also breaks, since newer setuptools removed pkg_resources
  which fontTools.ufoLib's fs dependency imports.
- The roman variable font builds in about 20 seconds from source on this
  machine, including the final bake-vf.py pass. `make var` builds only the
  two variable fonts; `make all` also builds 36 statics in several formats
  plus Google Fonts variants.
- The Dockerfile in misc/docker targets Python 3.7 and calls an init.sh that
  no longer exists in the repository. It does not match the current
  Pipfile-based setup.

What is cheap at the source level:

- Adding a static instance at a new weight: an entry in the instances array
  of both glyphspackage fontinfo.plist files, a name mapping in
  misc/tools/gen-instance-ufo.sh, and the style lists in the Makefile.
- Relabeling which instance is called Regular: the same files. The style
  name must stay literally "Regular", because
  misc/tools/postprocess-designspace.py keys its PostScript naming on that
  string.

What is expensive:

- Moving the variable font's default weight: the Variable Font Origin
  parameter must point at a real master, so this needs a new interpolation-
  compatible master at every optical size.
- Global spacing changes: 3,017 roman and 2,932 italic glyph files, each with
  six master layers. 825 roman glyphs use metric keys that derive a
  sidebearing from another glyph; the rest are set individually. The 2018
  history shows a uniform sidebearing change touching over a thousand glyph
  files for a single master.
- Making a variant the default: swapping outlines plus re-pointing kerning
  groups and every accented composite, per letter, in both roman and italic.
  The alternates are not drop-in replacements for the base outlines either:
  in the roman source, l has six master layers while l.ss02 has nine, and a
  has twenty while a.1 has seven, because intermediate layers differ per
  glyph. Interpolation compatibility has to be re-established per master.

## 11. Pitfalls

- opsz is not a tracking dial; it also lowers x-height and reshapes letters.
- Apply tracking to the unhinted variable fonts, not the hinted static TTFs in
  extras/ttf or Inter.ttc. Metric edits leave their hint programs stale.
- OpenType Feature Freezer handles only single and alternate substitutions,
  which covers every cv and ss feature but not ligatures or contextual
  alternates. It is silent on success. Its `-S`/`-U` suffix flags combined
  with `-R` produce doubled names when both contain the same word; rename with
  the repo's tool afterwards instead.
- The fontTools instancer leaves "Inter Variable" and "Regular" in the name
  table of a fully pinned instance unless `--update-name-table` is given.
  That flag only works when the pinned coordinates match STAT entries, and
  even then it was seen to leave the full name (ID 4) stale. Check the name
  table after instancing and fix it with the rename tool if needed.
- misc/tools/rename.py rewrites name IDs 1, 3, 4, 6, 16, and 21. It has
  code for name ID 25 but that code searches for the spaced family name
  inside the unspaced PostScript prefix and never matches on Inter's fonts,
  and it does not touch the per-instance PostScript name records at all. A
  font renamed with it alone still carries "InterVariable" in those strings.
- Uniform tracking on the variable font applies the same amount at every
  weight. Per-weight amounts need HVAR edits or per-weight statics.
- The feature inventory above is the v4.1 release. A build from the current
  source adds cv15 and cv16 and may differ in other details.

## 12. Sources

Repository (commit 353b61b9f):

- src/Inter-Roman.glyphspackage/fontinfo.plist: axes, masters, instances,
  Axis Mappings (avar), Variable Font Origin, embedded ss01/ss03/ss07/ss08.
- src/features/*.fea: hand-written cv and ss rules.
- misc/2023-06-06-regular-stroke-weight-increase.txt and commit b1fb2c8ed:
  the 2023 stem change. Commits 6a1af4f06, 2c652cfc9, d27040bf2, 70faa70bf:
  weight-mapping history.
- misc/tools/bake-vf.py, misc/tools/rename.py, misc/makezip2.sh: release
  post-processing and zip layout. Makefile targets var, var_web, all.
- docs/_data/faq.yml (faq-otfeat-customize-compile): the Feature Freezer
  recommendation. docs/lab/index.html: dynamic metrics constants.
- README.md: Reserved Font Name and trademark statement, notable forks.
- CONTRIBUTING.md: master compatibility rules, Display design notes.

External:

- https://github.com/rsms/inter/releases/tag/v4.1
- https://github.com/rsms/inter/issues/413 (tabular numbers by default: no,
  use the freezer)
- https://github.com/rsms/inter/issues/521 (stylistic alternates by default)
- https://github.com/rsms/inter/discussions/588 and
  https://github.com/googlefonts/inter-gf-tight (Inter Tight status)
- https://github.com/rsms/inter/discussions/734 (how Raveo was built)
- https://github.com/rsms/inter/discussions/771 (lighter Regular request,
  unanswered)
- https://github.com/twardoch/fonttools-opentype-feature-freezer
- https://fonttools.readthedocs.io/en/latest/varLib/instancer.html
