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
# vertical metrics that set the line height are unchanged.

import math
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, Protocol, cast

import pytest
from fontTools.pens.boundsPen import BoundsPen
from fontTools.pens.recordingPen import RecordingPen
from fontTools.ttLib import TTFont

if TYPE_CHECKING:
    from collections.abc import Callable, Iterable, Mapping

    from fontTools.pens.basePen import AbstractPen

REPO = Path(__file__).resolve().parents[1]
SOURCE_DIR = REPO / "commit-mono"
OUTPUT_DIR = REPO / "Library" / "Fonts"
FAMILY = "CommitMonoShields Nerd Font"

# Name records are compared on the Windows and Macintosh platforms; macOS
# reads the first and falls back to the second. Commit Mono itself only puts
# its copyright and license on the Windows platform.
WINDOWS_NAMES = (3, 1, 0x409)
MACINTOSH_NAMES = (1, 0, 0)
LICENSE_NAME_IDS = (0, 13, 14)

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
    # Copyright and license must carry over from Commit Mono.
    for name_id in LICENSE_NAME_IDS:
        expected[name_id] = fonts.source.name(name_id, WINDOWS_NAMES)
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
        if not in_patched_range(codepoint)
    ]
    assert not outside, summarize(outside)


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
