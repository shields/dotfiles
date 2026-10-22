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

# fontTools ships no type stubs; the Protocols below stand in for them.
# pyright: reportMissingTypeStubs=false

# Checks the fonts tools/create_nerd_commit_mono.sh builds into Library/Fonts/
# against their sources in commit-mono/. The patcher must add the Nerd Fonts
# glyphs at up to two cells wide, as the official CommitMono Nerd Font has
# them, while leaving the custom Commit Mono build alone: every glyph outside
# the ranges the patcher owns keeps its shape and advance, the OpenType
# substitutions that carry the chosen alternates are unchanged, and the
# vertical metrics that set the line height are unchanged. Running
# tools/add_commit_mono_glyph.py reproduces the glyphs it added to the fonts, so
# a change to the tool needs a rebuild.

import copy
import importlib.util
import math
import os
import re
import shutil
import subprocess
import sys
from enum import IntEnum
from hashlib import sha256
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, Protocol, cast

import pytest
from fontTools.pens.boundsPen import BoundsPen
from fontTools.pens.recordingPen import RecordingPen
from fontTools.pens.t2CharStringPen import T2CharStringPen
from fontTools.ttLib import TTFont

if TYPE_CHECKING:
    from collections.abc import Callable, Iterable, Mapping

    from fontTools.pens.basePen import AbstractPen

REPO = Path(__file__).resolve().parents[1]
SOURCE_DIR = REPO / "commit-mono"
OUTPUT_DIR = REPO / "Library" / "Fonts"
TOOL = REPO / "tools" / "add_commit_mono_glyph.py"
FAMILY = "CommitMonoShields Nerd Font"

# Name records are compared on the Windows and Macintosh platforms; macOS
# reads the first and falls back to the second. Commit Mono itself only puts
# its copyright and license on the Windows platform.
WINDOWS_NAMES = (3, 1, 0x409)
MACINTOSH_NAMES = (1, 0, 0)
LICENSE_NAME_IDS = (0, 13, 14)
DONOR_COPYRIGHT = (
    "Copyright 2022 The Noto Project Authors "
    "(https://github.com/notofonts/latin-greek-cyrillic)"
)
DONOR_DIR = SOURCE_DIR / "noto-sans-mono"
DOUBLE_OBLIQUE_HYPHEN = 0x2E17
OPEN_BOX = 0x2423
SHOULDERED_OPEN_BOX = 0x237D
ADDED_CODEPOINTS = (DOUBLE_OBLIQUE_HYPHEN, SHOULDERED_OPEN_BOX)
CELL_UNITS = 600

# Codepoint ranges the Nerd Fonts patcher writes into (setup_patch_set in
# font-patcher v3.5.1, with --complete on a monospaced font). Source glyphs in
# these ranges may be replaced; everything else must survive untouched.
PATCHED_RANGES = (
    (0x23FB, 0x23FE),  # IEC power symbols
    (0x2500, 0x259F),  # box drawing, replaced wholesale when any is missing
    (0x2630, 0x2630),  # trigram for heaven (Powerline Extra)
    (0x2665, 0x2665),  # heart (Octicons)
    (0x26A1, 0x26A1),  # zap (Octicons)
    (0x276C, 0x2771),  # heavy angle brackets
    (0x2800, 0x28FF),  # Braille
    (0x2B58, 0x2B58),  # heavy circle, IEC power off
    (0xE000, 0xE00A),  # Pomicons
    (0xE0A0, 0xE0D7),  # Powerline and Powerline Extra
    (0xE200, 0xE2A9),  # Font Awesome Extension
    (0xE300, 0xE3EB),  # Weather Icons
    (0xE5FA, 0xE6FF),  # Seti-UI and custom
    (0xE700, 0xE958),  # Devicons
    (0xEA60, 0xEC84),  # Codicons
    (0xED00, 0xF2FF),  # Font Awesome, including progress indicators at EE00
    (0xF300, 0xF385),  # Font Logos
    (0xF400, 0xF533),  # Octicons
    (0xF0001, 0xF1AF0),  # Material Design Icons
)

# One codepoint per symbol set, to catch a set silently dropping out.
SAMPLE_ICONS = {
    0x23FB: "IEC power",
    0x2630: "trigram for heaven",
    0x276C: "heavy angle bracket",
    0x2800: "Braille",
    0x2B58: "IEC power off",
    0xE000: "Pomicons",
    0xE0A0: "Powerline branch",
    0xE0B0: "Powerline right arrow",
    0xE0C0: "Powerline Extra flame",
    0xE200: "Font Awesome Extension",
    0xE300: "Weather Icons",
    0xE5FA: "Seti-UI",
    0xE700: "Devicons",
    0xEA60: "Codicons",
    0xED00: "Font Awesome",
    0xEE0B: "progress indicator",
    0xF300: "Font Logos",
    0xF400: "Octicons",
    0xF0001: "Material Design Icons",
}
MIN_ADDED_CODEPOINTS = 10_000

# The shouldered open box is the open box with its sides moved inward and an
# arm out from the top of each, drawn with the box's own strokes, so it weighs
# what the open box does; the tolerance leaves room for the taller sides.
WEIGHT_TOLERANCE = 0.05
# Slope of the slanted edges, in x per y, to within the rounding of integer
# coordinates on edges at least 68 units tall.
SLOPE_TOLERANCE = 0.01
ITALIC_SLOPE = 1 / 6
# How much narrower the box's bottom must be than the open box's, and how many
# times longer the ink along the top must be, for the two to differ at a glance.
MAX_BOTTOM_SHARE = 0.7
MIN_TOP_RUN_RATIO = 2

# Powerline separators are stretched to fill the whole cell so adjacent
# segments join seamlessly. This is why the build doesn't pass --careful:
# Commit Mono's own copies are sized for its default line height, not the
# custom one.
CELL_FILLING_ICONS = (0xE0B0, 0xE0B2, 0xE0B4, 0xE0B6)

# The Nerd Font (as opposed to Nerd Font Mono) variant keeps a one-cell advance
# but lets an icon's outline overhang into the next cell, and the patcher adds
# a little overlap to some Powerline glyphs beyond that.
MAX_ICON_WIDTH_CELLS = 2.1
MIN_TWO_CELL_ICON_SHARE = 0.5
# Vertical overshoot is limited to a hundredth of the cell by the patcher.
VERTICAL_TOLERANCE = 0.02

# The patcher's fontforge round trip rewrites a curve whose control points lie
# on its chord as a line, and drops a line back to a contour's start point
# since closing the contour draws it anyway. Coordinates are integers, so half
# a unit of slack covers rounding.
COLLINEAR_TOLERANCE = 0.5
CUBIC_POINTS = 3
# math.hypot(dx, dy) ** 2 doesn't always round-trip to exactly dx*dx + dy*dy,
# so a point exactly at the chord's start or end can land fractionally outside
# [0, 1]; this absorbs that float noise without accepting points meaningfully
# off the segment.
ALONG_TOLERANCE = 1e-9

FS_SELECTION_ITALIC = 1 << 0
FS_SELECTION_BOLD = 1 << 5
FS_SELECTION_REGULAR = 1 << 6
MAC_STYLE_BOLD = 1 << 0
MAC_STYLE_ITALIC = 1 << 1
MARK_GLYPH_CLASS = 3
NO_REQUIRED_FEATURE = 0xFFFF
CHAIN_CONTEXT_COVERAGE_FORMAT = 3

# GSUB LookupType values (OpenType spec).
GSUB_LOOKUP_SINGLE = 1
GSUB_LOOKUP_MULTIPLE = 2
GSUB_LOOKUP_ALTERNATE = 3
GSUB_LOOKUP_LIGATURE = 4
GSUB_LOOKUP_CHAIN_CONTEXT = 6
GSUB_LOOKUP_EXTENSION = 7

# 'name' table nameID values (OpenType spec) this file checks directly; the
# rest (copyright/license) are in LICENSE_NAME_IDS above.
NAME_ID_FAMILY = 1
NAME_ID_SUBFAMILY = 2
NAME_ID_FULL_NAME = 4
NAME_ID_VERSION = 5
NAME_ID_POSTSCRIPT_NAME = 6
NAME_ID_TYPOGRAPHIC_FAMILY = 16
NAME_ID_TYPOGRAPHIC_SUBFAMILY = 17

type Point = tuple[float, float]
type Segment = tuple[str, tuple[Point, ...]]
type Bounds = tuple[float, float, float, float]
# Nested tuples, frozensets, strings, and ints describing a lookup.
type Canonical = tuple[object, ...]


# fontTools builds its table objects dynamically, so these give the checks
# typed views of the attributes they read. Names mirror fontTools'.
class Drawable(Protocol):
    def draw(self, pen: AbstractPen) -> None: ...


class NameRecord(Protocol):
    def toUnicode(self) -> str: ...  # noqa: N802


class NameTable(Protocol):
    def getName(  # noqa: N802
        self,
        nameID: int,  # noqa: N803
        platformID: int,  # noqa: N803
        platEncID: int,  # noqa: N803
        langID: int,  # noqa: N803
    ) -> NameRecord | None: ...


class HmtxTable(Protocol):
    metrics: dict[str, tuple[int, int]]


class CmapSubtable(Protocol):
    platformID: int  # noqa: N815
    platEncID: int  # noqa: N815
    cmap: dict[int, str]


class CmapTable(Protocol):
    tables: list[CmapSubtable]


class TopDict(Protocol):
    CharStrings: dict[str, object]
    Private: object
    GlobalSubrs: object


class CffRoot(Protocol):
    topDictIndex: list[TopDict]  # noqa: N815


class CffTable(Protocol):
    cff: CffRoot


class Coverage(Protocol):
    glyphs: list[str]


class Ligature(Protocol):
    LigGlyph: str
    Component: list[str]


class SingleSubst(Protocol):
    mapping: dict[str, str]


class MultipleSubst(Protocol):
    mapping: dict[str, list[str]]


class AlternateSubst(Protocol):
    alternates: dict[str, list[str]]


class LigatureSubst(Protocol):
    ligatures: dict[str, list[Ligature]]


class SubstLookupRecord(Protocol):
    SequenceIndex: int
    LookupListIndex: int


class ChainContextSubst(Protocol):
    Format: int
    BacktrackCoverage: list[Coverage]
    InputCoverage: list[Coverage]
    LookAheadCoverage: list[Coverage]
    SubstLookupRecord: list[SubstLookupRecord]


class ExtensionSubst(Protocol):
    ExtensionLookupType: int
    ExtSubTable: object


class Lookup(Protocol):
    LookupType: int
    LookupFlag: int
    SubTable: list[object]


class LookupList(Protocol):
    Lookup: list[Lookup]


class Feature(Protocol):
    FeatureParams: object | None
    LookupListIndex: list[int]


class FeatureRecord(Protocol):
    FeatureTag: str
    Feature: Feature


class FeatureList(Protocol):
    FeatureRecord: list[FeatureRecord]


class LangSys(Protocol):
    ReqFeatureIndex: int
    FeatureIndex: list[int]


class LangSysRecord(Protocol):
    LangSysTag: str
    LangSys: LangSys


class Script(Protocol):
    DefaultLangSys: LangSys | None
    LangSysRecord: list[LangSysRecord]


class ScriptRecord(Protocol):
    ScriptTag: str
    Script: Script


class ScriptList(Protocol):
    ScriptRecord: list[ScriptRecord]


class LayoutRoot(Protocol):
    ScriptList: ScriptList | None
    FeatureList: FeatureList | None
    LookupList: LookupList | None


class LayoutTable(Protocol):
    table: LayoutRoot


class ClassDef(Protocol):
    classDefs: dict[str, int]  # noqa: N815


class GdefRoot(Protocol):
    GlyphClassDef: ClassDef | None


class GdefTable(Protocol):
    table: GdefRoot


class Face(NamedTuple):
    source: str
    style: str
    postscript: str
    bold: bool
    italic: bool

    @property
    def output(self) -> str:
        return f"CommitMonoShieldsNerdFont-{self.style.replace(' ', '')}.otf"

    @property
    def full_name(self) -> str:
        return FAMILY if self.style == "Regular" else f"{FAMILY} {self.style}"


FACES = (
    Face(
        source="CommitMonoShields-350-Regular",
        style="Regular",
        postscript="CommitMonoShieldsNF",
        bold=False,
        italic=False,
    ),
    Face(
        source="CommitMonoShields-350-Italic",
        style="Italic",
        postscript="CommitMonoShieldsNF-Italic",
        bold=False,
        italic=True,
    ),
    Face(
        source="CommitMonoShields-700-Regular",
        style="Bold",
        postscript="CommitMonoShieldsNF-Bold",
        bold=True,
        italic=False,
    ),
    Face(
        source="CommitMonoShields-700-Italic",
        style="Bold Italic",
        postscript="CommitMonoShieldsNF-BoldItalic",
        bold=True,
        italic=True,
    ),
)


class Font:
    """The parts of a font the checks look at, with the cell geometry."""

    def __init__(self, path: Path) -> None:
        self.path: Path = path
        self.ttf: TTFont = TTFont(path)
        self.cmap: dict[int, str] = self.ttf.getBestCmap() or {}
        self.glyph_set: Mapping[str, Drawable] = cast(
            "Mapping[str, Drawable]", self.ttf.getGlyphSet()
        )
        self.glyph_names: list[str] = self.ttf.getGlyphOrder()
        self.metrics: dict[str, tuple[int, int]] = cast(
            "HmtxTable", self.ttf["hmtx"]
        ).metrics
        self.ascent: float = self.field("hhea", "ascent")
        self.descent: float = self.field("hhea", "descent")
        self.height: float = self.ascent - self.descent
        # Commit Mono is monospaced, so any letter gives the cell width.
        self.cell: int = self.advance(self.cmap[ord("a")])

    def field(self, tag: str, name: str) -> float:
        return cast("float", getattr(self.ttf[tag], name))

    def advance(self, glyph: str) -> int:
        return self.metrics[glyph][0]

    def outline(self, glyph: str) -> list[Segment]:
        pen = RecordingPen()
        self.glyph_set[glyph].draw(pen)
        return straightened(cast("list[Segment]", pen.value))

    def bounds(self, glyph: str) -> Bounds | None:
        pen = BoundsPen(self.glyph_set)
        self.glyph_set[glyph].draw(pen)
        return cast("Bounds | None", pen.bounds)

    def name(self, name_id: int, platform: tuple[int, int, int]) -> str | None:
        record = cast("NameTable", self.ttf["name"]).getName(name_id, *platform)
        return None if record is None else record.toUnicode()

    def layout(self, tag: str) -> LayoutRoot | None:
        table = cast("LayoutTable | None", self.ttf.get(tag))
        return None if table is None else table.table


def polygon(font: Font, glyph: str) -> list[Point]:
    """The corners of a glyph that is one closed polygon of straight edges."""
    segments = font.outline(glyph)
    operators = [operator for operator, _ in segments]
    assert operators == ["moveTo", *["lineTo"] * (len(segments) - 2), "closePath"]
    return [points[0] for _, points in segments[:-1]]


def edges(corners: list[Point]) -> list[tuple[Point, Point]]:
    return list(zip(corners, [*corners[1:], corners[0]], strict=True))


def ink_weight(corners: list[Point]) -> float:
    """Twice the area over the perimeter, which is a stroke's width."""
    area = abs(sum(x0 * y1 - x1 * y0 for (x0, y0), (x1, y1) in edges(corners))) / 2
    perimeter = sum(math.dist(start, end) for start, end in edges(corners))
    return 2 * area / perimeter


def ink_runs(corners: list[Point], height: float) -> list[tuple[float, float]]:
    """The stretches of ink along a horizontal line."""
    crossings = sorted(
        x0 + (height - y0) * (x1 - x0) / (y1 - y0)
        for (x0, y0), (x1, y1) in edges(corners)
        if y0 <= height < y1 or y1 <= height < y0
    )
    return list(zip(crossings[::2], crossings[1::2], strict=True))


def in_patched_range(codepoint: int) -> bool:
    return any(low <= codepoint <= high for low, high in PATCHED_RANGES)


def summarize(items: Iterable[str], limit: int = 8) -> str:
    listed = list(items)
    shown = ", ".join(listed[:limit])
    if len(listed) > limit:
        shown += f", ... ({len(listed)} total)"
    return shown


def on_chord(start: Point, end: Point, point: Point) -> bool:
    """Whether point lies on the straight segment from start to end."""
    dx, dy = end[0] - start[0], end[1] - start[1]
    length = math.hypot(dx, dy)
    if length == 0:
        return point == start
    px, py = point[0] - start[0], point[1] - start[1]
    if abs(px * dy - py * dx) / length > COLLINEAR_TOLERANCE:
        return False
    along = (px * dx + py * dy) / (length * length)
    return -ALONG_TOLERANCE <= along <= 1 + ALONG_TOLERANCE


def straightened(segments: list[Segment]) -> list[Segment]:
    """Rewrite an outline without the redundancies fontforge removes."""
    result: list[Segment] = []
    current: Point | None = None
    start: Point | None = None
    for operator, points in segments:
        if (
            operator == "curveTo"
            and current is not None
            and len(points) == CUBIC_POINTS
            and all(on_chord(current, points[-1], point) for point in points[:-1])
        ):
            result.append(("lineTo", (points[-1],)))
        elif operator == "closePath" and result and result[-1] == ("lineTo", (start,)):
            result[-1] = (operator, points)
        else:
            result.append((operator, points))
        if operator == "moveTo":
            start = current = points[0]
        elif operator == "closePath":
            current = start
        elif points:
            current = points[-1]
    return result


def canonical_subtable(
    subtable: object, lookup_type: int, resolve: Callable[[int], Canonical]
) -> Canonical:
    """Describe a substitution subtable independently of table layout."""
    if lookup_type == GSUB_LOOKUP_SINGLE:
        single = cast("SingleSubst", subtable)
        return ("single", tuple(sorted(single.mapping.items())))
    if lookup_type == GSUB_LOOKUP_MULTIPLE:
        multiple = cast("MultipleSubst", subtable)
        return (
            "multiple",
            tuple(
                sorted((glyph, tuple(seq)) for glyph, seq in multiple.mapping.items())
            ),
        )
    if lookup_type == GSUB_LOOKUP_ALTERNATE:
        alternate = cast("AlternateSubst", subtable)
        return (
            "alternate",
            tuple(
                sorted(
                    (glyph, tuple(alts)) for glyph, alts in alternate.alternates.items()
                )
            ),
        )
    if lookup_type == GSUB_LOOKUP_LIGATURE:
        ligature = cast("LigatureSubst", subtable)
        # A first glyph's ligature rules are tried in list order (the first
        # whose components match wins), so only the first-glyph key is
        # sorted for determinism -- each glyph's rule list keeps its
        # original, semantically meaningful order.
        return (
            "ligature",
            tuple(
                sorted(
                    (first, tuple((tuple(lig.Component), lig.LigGlyph) for lig in ligs))
                    for first, ligs in ligature.ligatures.items()
                )
            ),
        )
    if lookup_type == GSUB_LOOKUP_CHAIN_CONTEXT:
        chain = cast("ChainContextSubst", subtable)
        if chain.Format != CHAIN_CONTEXT_COVERAGE_FORMAT:
            msg = f"chained context format {chain.Format} not handled"
            raise NotImplementedError(msg)
        return (
            "chain",
            tuple(frozenset(c.glyphs) for c in chain.BacktrackCoverage),
            tuple(frozenset(c.glyphs) for c in chain.InputCoverage),
            tuple(frozenset(c.glyphs) for c in chain.LookAheadCoverage),
            tuple(
                (r.SequenceIndex, resolve(r.LookupListIndex))
                for r in chain.SubstLookupRecord
            ),
        )
    if lookup_type == GSUB_LOOKUP_EXTENSION:
        extension = cast("ExtensionSubst", subtable)
        return canonical_subtable(
            extension.ExtSubTable, extension.ExtensionLookupType, resolve
        )
    msg = f"lookup type {lookup_type} not handled"
    raise NotImplementedError(msg)


def canonical_layout(root: LayoutRoot) -> dict[tuple[str, str], dict[str, Canonical]]:
    """Map each script and language to its features' substitutions.

    Lookups are described by content and nested lookups are inlined, so the
    result doesn't depend on how the lists are ordered or numbered.
    """
    lookups = [] if root.LookupList is None else root.LookupList.Lookup
    memo: dict[int, Canonical] = {}

    def resolve(index: int) -> Canonical:
        if index not in memo:
            lookup = lookups[index]
            memo[index] = (
                lookup.LookupFlag,
                tuple(
                    canonical_subtable(subtable, lookup.LookupType, resolve)
                    for subtable in lookup.SubTable
                ),
            )
        return memo[index]

    records = [] if root.FeatureList is None else root.FeatureList.FeatureRecord
    features: list[tuple[str, Canonical]] = [
        (
            record.FeatureTag,
            (
                record.Feature.FeatureParams is not None,
                tuple(resolve(index) for index in record.Feature.LookupListIndex),
            ),
        )
        for record in records
    ]
    scripts = [] if root.ScriptList is None else root.ScriptList.ScriptRecord
    result: dict[tuple[str, str], dict[str, Canonical]] = {}
    for script in scripts:
        systems = [
            ("dflt", script.Script.DefaultLangSys),
            *((r.LangSysTag, r.LangSys) for r in script.Script.LangSysRecord),
        ]
        for language, system in systems:
            if system is None:
                continue
            # A dict built straight from (tag, content) pairs would silently
            # keep only the last entry if a LangSys legally lists two
            # FeatureRecords sharing a tag; fail loudly instead unless
            # they're duplicates in content too.
            active: dict[str, Canonical] = {}
            for index in system.FeatureIndex:
                tag, content = features[index]
                if tag in active and active[tag] != content:
                    msg = (
                        f"{script.ScriptTag}/{language}: duplicate feature "
                        f"tag {tag!r} with different content"
                    )
                    raise NotImplementedError(msg)
                active[tag] = content
            if system.ReqFeatureIndex != NO_REQUIRED_FEATURE:
                tag, content = features[system.ReqFeatureIndex]
                active[f"required {tag}"] = content
            result[script.ScriptTag, language] = active
    return result


class Fonts(NamedTuple):
    face: Face
    source: Font
    patched: Font
    added: list[int]  # Codepoints the patcher added.


class IconMeasurements(NamedTuple):
    too_wide: list[str]
    too_tall: list[str]
    two_cells: int
    measured: int


@pytest.fixture(scope="module", params=FACES, ids=[face.style for face in FACES])
def fonts(request: pytest.FixtureRequest) -> Fonts:
    face = cast("Face", request.param)
    patched_path = OUTPUT_DIR / face.output
    if not patched_path.exists():
        pytest.fail(f"{patched_path} missing")
    source = Font(SOURCE_DIR / f"{face.source}.otf")
    patched = Font(patched_path)
    added = sorted(set(patched.cmap) - set(source.cmap))
    return Fonts(face, source, patched, added)


@pytest.fixture(scope="module")
def icons(fonts: Fonts) -> IconMeasurements:
    patched = fonts.patched
    too_wide: list[str] = []
    too_tall: list[str] = []
    two_cells = 0
    measured = 0
    top = patched.ascent + VERTICAL_TOLERANCE * patched.height
    bottom = patched.descent - VERTICAL_TOLERANCE * patched.height
    for codepoint in fonts.added:
        bounds = patched.bounds(patched.cmap[codepoint])
        if bounds is None:
            continue
        measured += 1
        x_min, y_min, x_max, y_max = bounds
        width = (x_max - x_min) / patched.cell
        if width > MAX_ICON_WIDTH_CELLS:
            too_wide.append(f"U+{codepoint:04X} ({width:.2f} cells)")
        elif width > 1:
            two_cells += 1
        if y_min < bottom or y_max > top:
            too_tall.append(f"U+{codepoint:04X} ({y_min:.0f}..{y_max:.0f})")
    return IconMeasurements(too_wide, too_tall, two_cells, measured)


def test_output_directory_holds_exactly_the_four_faces() -> None:
    expected = sorted(face.output for face in FACES)
    present = sorted(
        path.name for path in OUTPUT_DIR.glob("CommitMonoShieldsNerdFont*")
    )
    assert present == expected


name_platforms = pytest.mark.parametrize(
    "platform", [WINDOWS_NAMES, MACINTOSH_NAMES], ids=["windows", "macintosh"]
)


@name_platforms
def test_names(fonts: Fonts, platform: tuple[int, int, int]) -> None:
    expected: dict[int, str | None] = {
        NAME_ID_FAMILY: FAMILY,
        NAME_ID_SUBFAMILY: fonts.face.style,
        NAME_ID_FULL_NAME: fonts.face.full_name,
        NAME_ID_POSTSCRIPT_NAME: fonts.face.postscript,
    }
    for name_id in LICENSE_NAME_IDS:
        expected[name_id] = fonts.source.name(name_id, WINDOWS_NAMES)
    expected[0] = f"{expected[0]}\n\n{DONOR_COPYRIGHT}"
    expected[13] = f"{expected[13]}\n\n{(DONOR_DIR / 'OFL.txt').read_text().strip()}"
    actual = {name_id: fonts.patched.name(name_id, platform) for name_id in expected}
    assert actual == expected


@name_platforms
def test_typographic_family_agrees(
    fonts: Fonts, platform: tuple[int, int, int]
) -> None:
    # The typographic family, if the patcher wrote one, must agree -- a
    # property independent of test_names above, so it gets its own test
    # rather than sharing a function where an earlier failing assert would
    # hide this check.
    typographic = {
        name_id: fonts.patched.name(name_id, platform)
        for name_id, want in (
            (NAME_ID_TYPOGRAPHIC_FAMILY, FAMILY),
            (NAME_ID_TYPOGRAPHIC_SUBFAMILY, fonts.face.style),
        )
        if fonts.patched.name(name_id, platform) not in (None, want)
    }
    assert not typographic


def test_version_keeps_commit_monos(fonts: Fonts) -> None:
    version = fonts.patched.name(NAME_ID_VERSION, WINDOWS_NAMES) or ""
    source_version = fonts.source.name(NAME_ID_VERSION, WINDOWS_NAMES) or ""
    # An empty source_version would otherwise pass vacuously ("" in x).
    assert source_version
    assert source_version in version


def test_style_flags(fonts: Fonts) -> None:
    selection = int(fonts.patched.field("OS/2", "fsSelection"))
    mac_style = int(fonts.patched.field("head", "macStyle"))
    flags = {
        "fsSelection italic": bool(selection & FS_SELECTION_ITALIC),
        "fsSelection bold": bool(selection & FS_SELECTION_BOLD),
        "fsSelection regular": bool(selection & FS_SELECTION_REGULAR),
        "macStyle bold": bool(mac_style & MAC_STYLE_BOLD),
        "macStyle italic": bool(mac_style & MAC_STYLE_ITALIC),
    }
    expected = {
        "fsSelection italic": fonts.face.italic,
        "fsSelection bold": fonts.face.bold,
        "fsSelection regular": not fonts.face.bold and not fonts.face.italic,
        "macStyle bold": fonts.face.bold,
        "macStyle italic": fonts.face.italic,
    }
    assert flags == expected


def test_metrics_unchanged(fonts: Fonts) -> None:
    fields = (
        ("head", "unitsPerEm"),
        ("hhea", "ascent"),
        ("hhea", "descent"),
        ("hhea", "lineGap"),
        ("OS/2", "sTypoAscender"),
        ("OS/2", "sTypoDescender"),
        ("OS/2", "sTypoLineGap"),
        ("OS/2", "usWinAscent"),
        ("OS/2", "usWinDescent"),
        ("OS/2", "usWeightClass"),
        ("OS/2", "usWidthClass"),
        ("OS/2", "xAvgCharWidth"),
        ("OS/2", "sxHeight"),
        ("OS/2", "sCapHeight"),
        ("post", "italicAngle"),
        ("post", "isFixedPitch"),
        ("post", "underlinePosition"),
        ("post", "underlineThickness"),
    )
    changed = {
        f"{tag}.{name}": (fonts.source.field(tag, name), fonts.patched.field(tag, name))
        for tag, name in fields
        if fonts.source.field(tag, name) != fonts.patched.field(tag, name)
    }
    assert not changed


def test_still_fixed_pitch(fonts: Fonts) -> None:
    assert fonts.patched.field("post", "isFixedPitch") == 1


@pytest.fixture(scope="module")
def kept_glyphs(fonts: Fonts) -> list[str]:
    """Glyphs outside the codepoint ranges the patcher is allowed to replace."""
    replaceable = {
        glyph
        for codepoint, glyph in fonts.source.cmap.items()
        if in_patched_range(codepoint)
    }
    return [glyph for glyph in fonts.source.glyph_names if glyph not in replaceable]


def test_glyphs_outside_patched_ranges_are_present(
    fonts: Fonts, kept_glyphs: list[str]
) -> None:
    missing = [glyph for glyph in kept_glyphs if glyph not in fonts.patched.glyph_set]
    assert not missing, f"missing {summarize(missing)}"


def test_glyphs_outside_patched_ranges_keep_their_shape(
    fonts: Fonts, kept_glyphs: list[str]
) -> None:
    source, patched = fonts.source, fonts.patched
    # Independent of test_glyphs_outside_patched_ranges_are_present above, so
    # a glyph missing from patched is skipped here rather than relying on the
    # other test having already caught it.
    changed = [
        glyph
        for glyph in kept_glyphs
        if glyph in patched.glyph_set
        and (source.outline(glyph), source.advance(glyph))
        != (patched.outline(glyph), patched.advance(glyph))
    ]
    assert not changed, f"changed {summarize(changed)}"


def test_every_source_codepoint_still_encoded(fonts: Fonts) -> None:
    lost = [
        f"U+{codepoint:04X}"
        for codepoint in fonts.source.cmap
        if codepoint not in fonts.patched.cmap
    ]
    assert not lost, f"lost {summarize(lost)}"


def test_gsub_substitutions_unchanged(fonts: Fonts) -> None:
    source_gsub = fonts.source.layout("GSUB")
    patched_gsub = fonts.patched.layout("GSUB")
    assert source_gsub is not None
    assert patched_gsub is not None
    before = canonical_layout(source_gsub)
    after = canonical_layout(patched_gsub)
    differing = [
        f"{script}/{language}: {tag}"
        for (script, language), features in before.items()
        for tag, content in features.items()
        if after.get((script, language), {}).get(tag) != content
    ]
    assert before == after, (
        summarize(differing) or f"{sorted(before)} vs {sorted(after)}"
    )


# Commit Mono has no GPOS or GDEF. The patcher's fontforge round trip may
# add them, but they must not position or reclassify the original glyphs.
def test_no_gpos_lookups(fonts: Fonts) -> None:
    gpos = fonts.patched.layout("GPOS")
    lookups = (
        0 if gpos is None or gpos.LookupList is None else len(gpos.LookupList.Lookup)
    )
    assert lookups == 0


def test_no_original_glyph_reclassified_as_a_mark(fonts: Fonts) -> None:
    gdef = cast("GdefTable | None", fonts.patched.ttf.get("GDEF"))
    class_defs = (
        {}
        if gdef is None or gdef.table.GlyphClassDef is None
        else gdef.table.GlyphClassDef.classDefs
    )
    marks = [
        glyph
        for glyph in fonts.source.glyph_names
        if class_defs.get(glyph) == MARK_GLYPH_CLASS
    ]
    assert not marks, summarize(marks)


def test_enough_codepoints_added(fonts: Fonts) -> None:
    assert len(fonts.added) >= MIN_ADDED_CODEPOINTS


def test_every_symbol_set_present(fonts: Fonts) -> None:
    absent = [
        f"U+{codepoint:04X} ({label})"
        for codepoint, label in SAMPLE_ICONS.items()
        if codepoint not in fonts.patched.cmap
    ]
    assert not absent, summarize(absent)


def test_additions_stay_inside_the_patchers_ranges(fonts: Fonts) -> None:
    outside = [
        f"U+{codepoint:04X}"
        for codepoint in fonts.added
        if not in_patched_range(codepoint) and codepoint not in ADDED_CODEPOINTS
    ]
    assert not outside, summarize(outside)


def test_donor_subset_matches_documented_source() -> None:
    donor_path = DONOR_DIR / "NotoSansMono-DoubleObliqueHyphen.ttf"
    assert sha256(donor_path.read_bytes()).hexdigest() == (
        "79a7324d5e446579a82c3dc2b7d189b48ff08485c041f205e7fa05e4e3220d5b"
    )
    with TTFont(donor_path) as donor:
        assert set(donor.getBestCmap() or {}) == {DOUBLE_OBLIQUE_HYPHEN}


def test_double_oblique_hyphen_has_two_sloping_strokes(fonts: Fonts) -> None:
    patched = fonts.patched
    glyph = patched.cmap[DOUBLE_OBLIQUE_HYPHEN]
    expected_bounds = {
        "Regular": (148, 81, 452, 481),
        "Italic": (119, 81, 474, 481),
        "Bold": (147, 74, 453, 490),
        "Bold Italic": (118, 74, 476, 490),
    }
    assert patched.bounds(glyph) == expected_bounds[fonts.face.style]
    outline = patched.outline(glyph)
    assert [operator for operator, _ in outline] == [
        "moveTo",
        "lineTo",
        "lineTo",
        "lineTo",
        "closePath",
        "moveTo",
        "lineTo",
        "lineTo",
        "lineTo",
        "closePath",
    ]
    for start in (0, 5):
        points = [outline[start + i][1][0] for i in range(4)]
        left, right = points[1:3]
        assert right[0] > left[0]
        assert right[1] > left[1]
        assert all(0 < x < patched.cell for x, _ in points)
        assert all(0 < y < patched.ascent for _, y in points)


@pytest.mark.parametrize("codepoint", ADDED_CODEPOINTS, ids=hex)
def test_added_glyph_is_present_and_one_cell_wide(fonts: Fonts, codepoint: int) -> None:
    patched = fonts.patched
    assert codepoint in fonts.added
    assert patched.advance(patched.cmap[codepoint]) == CELL_UNITS == patched.cell


class OpenBoxes(NamedTuple):
    box: list[Point]
    shouldered: list[Point]
    top: float
    bottom: float
    stroke: float


def open_boxes(fonts: Fonts) -> OpenBoxes:
    patched = fonts.patched
    assert SHOULDERED_OPEN_BOX in patched.cmap, "U+237D is missing"
    box = polygon(patched, patched.cmap[OPEN_BOX])
    shouldered = polygon(patched, patched.cmap[SHOULDERED_OPEN_BOX])
    bottom, inner_bottom, top = sorted({y for _, y in box})
    return OpenBoxes(box, shouldered, top, bottom, inner_bottom - bottom)


def test_shouldered_open_box_spans_the_open_box(fonts: Fonts) -> None:
    box, shouldered, top, bottom, _ = open_boxes(fonts)
    patched = fonts.patched
    expected_bounds = {
        "Regular": (34, -168, 566, 68),
        "Italic": (-16, -168, 527, 68),
        "Bold": (20, -200, 580, 100),
        "Bold Italic": (-30, -200, 546, 100),
    }
    assert (
        patched.bounds(patched.cmap[SHOULDERED_OPEN_BOX])
        == expected_bounds[fonts.face.style]
    )
    assert min(y for _, y in shouldered) == bottom
    box_runs = ink_runs(box, top - 1)
    shouldered_runs = ink_runs(shouldered, top + 1)
    assert shouldered_runs[0][0] == pytest.approx(box_runs[0][0], abs=1)
    assert shouldered_runs[-1][1] == pytest.approx(box_runs[-1][1], abs=1)


def test_shouldered_open_box_rests_its_arms_on_the_open_boxs_top(
    fonts: Fonts,
) -> None:
    _, shouldered, top, bottom, stroke = open_boxes(fonts)
    assert sorted({y for _, y in shouldered}) == [
        bottom,
        bottom + stroke,
        top,
        top + stroke,
    ]


def test_shouldered_open_box_has_the_open_boxs_weight(fonts: Fonts) -> None:
    boxes = open_boxes(fonts)
    assert ink_weight(boxes.shouldered) == pytest.approx(
        ink_weight(boxes.box), rel=WEIGHT_TOLERANCE
    )


def test_shouldered_open_box_has_the_open_boxs_slant(fonts: Fonts) -> None:
    expected = ITALIC_SLOPE if fonts.face.italic else 0
    boxes = open_boxes(fonts)
    for corners in (boxes.box, boxes.shouldered):
        slopes = [
            (x1 - x0) / (y1 - y0) for (x0, y0), (x1, y1) in edges(corners) if y0 != y1
        ]
        assert slopes == pytest.approx([expected] * len(slopes), abs=SLOPE_TOLERANCE)


def test_shouldered_open_box_differs_from_the_open_box(fonts: Fonts) -> None:
    box, shouldered, top, bottom, _ = open_boxes(fonts)
    (box_bottom,) = ink_runs(box, bottom + 1)
    (shouldered_bottom,) = ink_runs(shouldered, bottom + 1)
    assert shouldered_bottom[1] - shouldered_bottom[0] <= MAX_BOTTOM_SHARE * (
        box_bottom[1] - box_bottom[0]
    )
    box_left = ink_runs(box, top - 1)[0]
    shouldered_left = ink_runs(shouldered, top + 1)[0]
    assert shouldered_left[1] - shouldered_left[0] >= MIN_TOP_RUN_RATIO * (
        box_left[1] - box_left[0]
    )


def run_tool(*fonts: Path) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        [sys.executable, str(TOOL), *map(str, fonts)],
        check=False,
        capture_output=True,
        timeout=60,
    )


@pytest.fixture(scope="module")
def grafted(fonts: Fonts, tmp_path_factory: pytest.TempPathFactory) -> list[Path]:
    directory = tmp_path_factory.mktemp(fonts.face.output)
    outputs = [directory / "first.otf", directory / "second.otf"]
    for output in outputs:
        _ = shutil.copyfile(fonts.source.path, output)
    result = run_tool(*outputs)
    assert result.returncode == 0, result.stderr
    return outputs


def test_graft_preserves_all_existing_glyphs_and_is_repeatable(
    fonts: Fonts, grafted: list[Path], tmp_path: Path
) -> None:
    assert grafted[0].read_bytes() == grafted[1].read_bytes()
    rebuilt = Font(grafted[0])
    assert set(rebuilt.cmap) - set(fonts.source.cmap) == set(ADDED_CODEPOINTS)
    assert all(
        rebuilt.outline(glyph) == fonts.source.outline(glyph)
        and rebuilt.advance(glyph) == fonts.source.advance(glyph)
        for glyph in fonts.source.glyph_names
    )
    again = tmp_path / "again.otf"
    _ = shutil.copyfile(grafted[0], again)
    before = again.read_bytes()
    result = run_tool(again)
    assert result.returncode != 0
    assert b"U+2E17 already present" in result.stderr
    assert again.read_bytes() == before


# The patcher writes nowhere near U+2E17, U+237D, or U+2423, so the tool draws
# the same glyphs on a source font as on the patched one.
def test_graft_reproduces_the_committed_additions(
    fonts: Fonts, grafted: list[Path]
) -> None:
    def additions(font: Font) -> dict[int, tuple[str, list[Segment], tuple[int, int]]]:
        return {
            codepoint: (
                font.cmap[codepoint],
                font.outline(font.cmap[codepoint]),
                font.metrics[font.cmap[codepoint]],
            )
            for codepoint in ADDED_CODEPOINTS
        }

    assert additions(Font(grafted[0])) == additions(fonts.patched)


def test_graft_requires_the_open_box(tmp_path: Path) -> None:
    source = tmp_path / "source.otf"
    with TTFont(SOURCE_DIR / f"{FACES[0].source}.otf") as font:
        for table in cast("CmapTable", font["cmap"]).tables:
            _ = table.cmap.pop(OPEN_BOX, None)
        font.save(source)
    result = run_tool(source)
    assert result.returncode != 0
    assert b"U+2423 is missing" in result.stderr


def test_graft_maps_every_unicode_cmap_subtable(tmp_path: Path) -> None:
    source = tmp_path / "source.otf"
    with TTFont(SOURCE_DIR / f"{FACES[0].source}.otf") as font:
        tables = cast("CmapTable", font["cmap"]).tables
        (original,) = tables
        macintosh = {ord("a"): original.cmap[ord("a")]}
        # Equal subtables share one dict once loaded, so each of these has
        # contents of its own.
        contents = {
            (0, 3): {**original.cmap, 0xE000: "space"},
            (1, 0): macintosh,
            (3, 10): {**original.cmap, 0xE001: "space"},
        }
        for (platform, encoding), cmap in contents.items():
            twin = copy.deepcopy(original)
            twin.platformID, twin.platEncID, twin.cmap = platform, encoding, cmap
            tables.append(twin)
        font.save(source)
    result = run_tool(source)
    assert result.returncode == 0, result.stderr
    with TTFont(source) as font:
        tables = cast("CmapTable", font["cmap"]).tables
        mapped = {
            (table.platformID, table.platEncID): {
                codepoint: table.cmap.get(codepoint) for codepoint in ADDED_CODEPOINTS
            }
            for table in tables
        }
        unmapped = dict.fromkeys(ADDED_CODEPOINTS)
        glyphs = mapped[3, 1]
        assert None not in glyphs.values()
        assert mapped == {
            (0, 3): glyphs,
            (1, 0): unmapped,
            (3, 1): glyphs,
            (3, 10): glyphs,
        }
        assert next(t for t in tables if t.platformID == 1).cmap == macintosh


class GlyphTool(Protocol):
    def shouldered_open_box(
        self, font: TTFont, cmap: dict[int, str]
    ) -> Callable[[AbstractPen], None]: ...


@pytest.fixture(scope="module")
def tool() -> GlyphTool:
    spec = importlib.util.spec_from_file_location("add_commit_mono_glyph", TOOL)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return cast("GlyphTool", cast("object", module))


class Corner(IntEnum):
    """Index of each point of U+2423, which starts at the outer bottom left."""

    OUTER_BOTTOM_LEFT = 0
    OUTER_BOTTOM_RIGHT = 1
    OUTER_TOP_RIGHT = 2
    INNER_TOP_RIGHT = 3
    INNER_BOTTOM_RIGHT = 4
    INNER_BOTTOM_LEFT = 5
    INNER_TOP_LEFT = 6
    OUTER_TOP_LEFT = 7


type Mutation = Callable[[list[Segment]], list[Segment]]
type Move = tuple[Corner, int, int]


def moving(*moves: Move) -> Mutation:
    def mutate(outline: list[Segment]) -> list[Segment]:
        result = list(outline)
        for corner, dx, dy in moves:
            operator, ((x, y),) = result[corner]
            result[corner] = (operator, ((x + dx, y + dy),))
        return result

    return mutate


def with_seven_points(outline: list[Segment]) -> list[Segment]:
    return [*outline[: Corner.INNER_TOP_LEFT], *outline[Corner.INNER_TOP_LEFT + 1 :]]


def with_nine_points(outline: list[Segment]) -> list[Segment]:
    return [
        *outline[: Corner.OUTER_TOP_LEFT + 1],
        # Off the line through its neighbors, which the encoder would drop.
        ("lineTo", ((300, 50),)),
        *outline[Corner.OUTER_TOP_LEFT + 1 :],
    ]


def with_curve(outline: list[Segment]) -> list[Segment]:
    _, (corner,) = outline[Corner.INNER_TOP_RIGHT]
    return [
        *outline[: Corner.INNER_TOP_RIGHT],
        ("curveTo", (corner, corner, corner)),
        *outline[Corner.INNER_TOP_RIGHT + 1 :],
    ]


def assert_rejected(tool: GlyphTool, mutate: Mutation, message: str) -> None:
    font = Font(SOURCE_DIR / f"{FACES[0].source}.otf")
    glyph = font.cmap[OPEN_BOX]
    recording = RecordingPen()
    recording.value = mutate(font.outline(glyph))
    pen = T2CharStringPen(0, None)
    cast("Callable[[AbstractPen], None]", recording.replay)(pen)
    top = cast("CffTable", cast("object", font.ttf["CFF "])).cff.topDictIndex[0]
    top.CharStrings[glyph] = cast(
        "Callable[[object, object], object]", pen.getCharString
    )(top.Private, top.GlobalSubrs)
    with pytest.raises(ValueError, match=re.escape(message)):
        _ = tool.shouldered_open_box(font.ttf, font.cmap)


@pytest.mark.parametrize(
    "mutate",
    [
        pytest.param(with_seven_points, id="seven points"),
        pytest.param(with_nine_points, id="nine points"),
        pytest.param(with_curve, id="curve"),
    ],
)
def test_shouldered_open_box_rejects_other_polygons(
    tool: GlyphTool, mutate: Mutation
) -> None:
    assert_rejected(tool, mutate, "U+2423 is not a polygon of eight points")


@pytest.mark.parametrize(
    "mutate",
    [
        pytest.param(moving((Corner.OUTER_BOTTOM_RIGHT, 0, 1)), id="uneven bottom"),
        pytest.param(moving((Corner.INNER_BOTTOM_RIGHT, 0, 1)), id="uneven bar"),
        pytest.param(moving((Corner.INNER_TOP_RIGHT, 0, -1)), id="uneven inner right"),
        pytest.param(moving((Corner.INNER_TOP_LEFT, 0, -1)), id="uneven inner left"),
        pytest.param(moving((Corner.OUTER_TOP_LEFT, 0, -1)), id="uneven outer left"),
        pytest.param(
            moving(
                (Corner.INNER_BOTTOM_RIGHT, 0, -1000),
                (Corner.INNER_BOTTOM_LEFT, 0, -1000),
            ),
            id="bar below the bottom",
        ),
        pytest.param(
            moving(
                (Corner.INNER_BOTTOM_RIGHT, 0, 1000),
                (Corner.INNER_BOTTOM_LEFT, 0, 1000),
            ),
            id="bar above the top",
        ),
        pytest.param(moving((Corner.INNER_BOTTOM_RIGHT, -1000, 0)), id="crossed sides"),
        pytest.param(
            moving((Corner.INNER_BOTTOM_LEFT, -1000, 0)), id="left side outside the box"
        ),
        pytest.param(
            moving((Corner.INNER_BOTTOM_RIGHT, 1000, 0)),
            id="right side outside the box",
        ),
    ],
)
def test_shouldered_open_box_rejects_other_eight_point_shapes(
    tool: GlyphTool, mutate: Mutation
) -> None:
    assert_rejected(tool, mutate, "U+2423 is not an open box")


def test_rebuild_with_read_only_patcher_outputs(tmp_path: Path) -> None:
    repo = tmp_path / "repo"
    tools = repo / "tools"
    tools.mkdir(parents=True)
    for name in ("create_nerd_commit_mono.sh", "add_commit_mono_glyph.py"):
        _ = shutil.copyfile(REPO / "tools" / name, tools / name)
    _ = shutil.copytree(DONOR_DIR, repo / "commit-mono" / DONOR_DIR.name)
    source = SOURCE_DIR / f"{FACES[0].source}.otf"
    _ = shutil.copyfile(source, repo / "commit-mono" / source.name)

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    stubs = {
        "docker": """
import shutil
import sys
from pathlib import Path

mounts = {}
for index, arg in enumerate(sys.argv):
    if arg == "--volume":
        host, container = sys.argv[index + 1].rsplit(":", 1)
        mounts[container] = Path(host)
for source in mounts["/in"].glob("*.otf"):
    output = mounts["/out"] / source.name
    shutil.copyfile(source, output)
    output.chmod(0o444)
""",
        "uv": """
import os
import stat
import sys
from pathlib import Path

for arg in sys.argv[7:]:
    info = Path(arg).stat()
    assert info.st_uid == os.getuid()
    assert info.st_mode & stat.S_IWUSR
os.execv(sys.executable, [sys.executable, *sys.argv[6:]])
""",
    }
    for name, body in stubs.items():
        stub = bin_dir / name
        _ = stub.write_text(f"#!{sys.executable}\n{body}")
        stub.chmod(0o755)

    result = subprocess.run(
        ["/bin/bash", str(tools / "create_nerd_commit_mono.sh")],
        env=os.environ | {"PATH": f"{bin_dir}{os.pathsep}{os.environ['PATH']}"},
        capture_output=True,
        text=True,
        check=False,
        timeout=60,
    )
    assert result.returncode == 0, result.stderr
    outputs = list((repo / "Library" / "Fonts").glob("*.otf"))
    assert len(outputs) == 1
    with TTFont(outputs[0]) as font:
        assert set(ADDED_CODEPOINTS) <= set(font.getBestCmap() or {})


def test_every_glyph_advances_one_cell(fonts: Fonts) -> None:
    patched = fonts.patched
    wrong_advance = [
        glyph for glyph in patched.glyph_names if patched.advance(glyph) != patched.cell
    ]
    assert not wrong_advance, f"cell is {patched.cell}; {summarize(wrong_advance)}"


def test_no_icon_wider_than_two_cells(icons: IconMeasurements) -> None:
    assert not icons.too_wide, summarize(icons.too_wide)


def test_icons_overhang_into_a_second_cell(icons: IconMeasurements) -> None:
    # measured=0 would otherwise pass vacuously (0 >= 0.5 * 0).
    assert icons.measured > 0
    assert icons.two_cells >= MIN_TWO_CELL_ICON_SHARE * icons.measured, (
        f"only {icons.two_cells} of {icons.measured} icons are wider than one cell"
    )


def test_icons_stay_within_the_line_height(icons: IconMeasurements) -> None:
    assert not icons.too_tall, summarize(icons.too_tall)


def test_powerline_separators_fill_the_cell(fonts: Fonts) -> None:
    patched = fonts.patched
    not_filling: list[str] = []
    for codepoint in CELL_FILLING_ICONS:
        if codepoint not in patched.cmap:
            not_filling.append(f"U+{codepoint:04X} (missing)")
            continue
        bounds = patched.bounds(patched.cmap[codepoint])
        if bounds is None:
            not_filling.append(f"U+{codepoint:04X} (empty)")
            continue
        x_min, y_min, x_max, y_max = bounds
        if (
            y_min > patched.descent
            or y_max < patched.ascent
            or x_max - x_min < patched.cell
        ):
            box = f"({x_min:.0f},{y_min:.0f})..({x_max:.0f},{y_max:.0f})"
            not_filling.append(f"U+{codepoint:04X} {box}")
    cell = f"{patched.cell} x {patched.descent:.0f}..{patched.ascent:.0f}"
    assert not not_filling, f"cell is {cell}; {summarize(not_filling)}"
