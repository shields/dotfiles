#!/usr/bin/env python
# pyright: reportMissingTypeStubs=false
# ruff: noqa: CPY001

import argparse
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, Protocol, cast

from fontTools.misc.transform import Transform
from fontTools.pens.boundsPen import BoundsPen
from fontTools.pens.recordingPen import RecordingPen
from fontTools.pens.t2CharStringPen import T2CharStringPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

if TYPE_CHECKING:
    from collections.abc import Callable, Mapping

    from fontTools.misc.psCharStrings import T2CharString
    from fontTools.pens.basePen import AbstractPen

DONOR_DIR = Path(__file__).resolve().parents[1] / "commit-mono" / "noto-sans-mono"
DONOR_FONT = DONOR_DIR / "NotoSansMono-DoubleObliqueHyphen.ttf"
ITALIC_SHEAR = 1 / 6
OPEN_BOX = 0x2423
# U+2423 spans nearly the whole cell, so the shoulders' length comes out of the
# box's width; each shoulder takes this share of it.
SHOULDER_SHARE = 1 / 5


class Addition(NamedTuple):
    codepoint: int
    glyph: str


DOUBLE_OBLIQUE_HYPHEN = Addition(0x2E17, "uni2E17")
SHOULDERED_OPEN_BOX = Addition(0x237D, "uni237D")
type Draw = Callable[[AbstractPen], None]
type Point = tuple[float, float]


class Drawable(Protocol):
    def draw(self, pen: AbstractPen) -> None: ...


class Head(Protocol):
    unitsPerEm: int  # noqa: N815


class OS2(Protocol):
    usWeightClass: int  # noqa: N815
    fsSelection: int  # noqa: N815

    def recalcUnicodeRanges(self, font: TTFont) -> set[int]: ...  # noqa: N802


class Cmap(Protocol):
    cmap: dict[int, str]

    def isUnicode(self) -> bool: ...  # noqa: N802


class Cmaps(Protocol):
    tables: list[Cmap]


class Metrics(Protocol):
    metrics: dict[str, tuple[int, int]]


class CharStringIndex(Protocol):
    def append(self, charstring: T2CharString) -> None: ...
    def __len__(self) -> int: ...


class CharStrings(Protocol):
    charStrings: dict[str, int]  # noqa: N815
    charStringsIndex: CharStringIndex  # noqa: N815


class Private(Protocol):
    nominalWidthX: int  # noqa: N815


class TopDict(Protocol):
    CharStrings: CharStrings
    Private: Private
    GlobalSubrs: list[T2CharString]
    charset: list[str]


class CFF(Protocol):
    topDictIndex: list[TopDict]  # noqa: N815


class CFFTable(Protocol):
    cff: CFF


class NameRecord(Protocol):
    nameID: int  # noqa: N815
    platformID: int  # noqa: N815
    platEncID: int  # noqa: N815
    langID: int  # noqa: N815

    def toUnicode(self) -> str: ...  # noqa: N802


class Names(Protocol):
    names: list[NameRecord]

    def getDebugName(self, name_id: int) -> str | None: ...  # noqa: N802
    def setName(  # noqa: N802
        self,
        string: str,
        name_id: int,
        platform_id: int,
        encoding_id: int,
        language_id: int,
    ) -> None: ...


def double_oblique_hyphen(font: TTFont, donor_font: TTFont, cell: int) -> Draw:
    style = cast("OS2", cast("object", font["OS/2"]))
    instantiate = cast(
        "Callable[[TTFont, dict[str, int]], TTFont]",
        instancer.instantiateVariableFont,
    )
    donor = instantiate(donor_font, {"wght": style.usWeightClass, "wdth": 100})
    donor_cmap = donor.getBestCmap()
    if donor_cmap is None or DOUBLE_OBLIQUE_HYPHEN.codepoint not in donor_cmap:
        msg = "Noto Sans Mono donor lacks U+2E17"
        raise ValueError(msg)
    donor_glyph = donor_cmap[DOUBLE_OBLIQUE_HYPHEN.codepoint]
    glyphs = cast("Mapping[str, Drawable]", donor.getGlyphSet())
    donor_cell = cast("Metrics", donor["hmtx"]).metrics[donor_glyph][0]
    if (
        cast("Head", cast("object", font["head"])).unitsPerEm
        != cast("Head", cast("object", donor["head"])).unitsPerEm
        or cell != donor_cell
    ):
        msg = "donor and target cell geometry differ"
        raise ValueError(msg)

    transform = Transform()
    if style.fsSelection & 1:
        # The italic metadata is near zero; the outlines use a 1:6 shear.
        transform = Transform(1, 0, ITALIC_SHEAR, 1, -cell * ITALIC_SHEAR / 2, 0)

    def draw(pen: AbstractPen) -> None:
        glyphs[donor_glyph].draw(TransformPen(pen, transform))

    return draw


def shouldered_open_box(font: TTFont, cmap: dict[int, str]) -> Draw:
    if OPEN_BOX not in cmap:
        msg = "U+2423 is missing"
        raise ValueError(msg)
    record = RecordingPen()
    cast("Mapping[str, Drawable]", font.getGlyphSet())[cmap[OPEN_BOX]].draw(record)
    recorded = cast("list[tuple[str, tuple[Point, ...]]]", record.value)
    if [operator for operator, _ in recorded] != [
        "moveTo",
        *["lineTo"] * 7,
        "closePath",
    ]:
        msg = "U+2423 is not a polygon of eight points"
        raise ValueError(msg)
    (
        outer_bottom_left,
        outer_bottom_right,
        outer_top_right,
        inner_top_right,
        inner_bottom_right,
        inner_bottom_left,
        inner_top_left,
        outer_top_left,
    ) = (points[0] for _, points in recorded[:8])
    if not (
        outer_bottom_left[1] == outer_bottom_right[1]
        and inner_bottom_right[1] == inner_bottom_left[1]
        and outer_top_right[1]
        == inner_top_right[1]
        == inner_top_left[1]
        == outer_top_left[1]
        and outer_bottom_left[1] < inner_bottom_right[1] < outer_top_right[1]
        and outer_bottom_left[0]
        < inner_bottom_left[0]
        < inner_bottom_right[0]
        < outer_bottom_right[0]
    ):
        msg = "U+2423 is not an open box"
        raise ValueError(msg)

    shear = (outer_top_left[0] - outer_bottom_left[0]) / (
        outer_top_left[1] - outer_bottom_left[1]
    )
    shoulder = round((outer_bottom_right[0] - outer_bottom_left[0]) * SHOULDER_SHARE)
    top = outer_top_right[1]
    bar = inner_bottom_right[1] - outer_bottom_right[1]

    def at(point: Point, height: float) -> Point:
        return (point[0] + shear * (height - point[1]), height)

    stem_left = (outer_bottom_left[0] + shoulder, outer_bottom_left[1])
    stem_right = (outer_bottom_right[0] - shoulder, outer_bottom_right[1])
    outline = [
        stem_left,
        stem_right,
        at(stem_right, top),
        at(outer_bottom_right, top),
        at(outer_bottom_right, top + bar),
        at((inner_top_right[0] - shoulder, top), top + bar),
        (inner_bottom_right[0] - shoulder, inner_bottom_right[1]),
        (inner_bottom_left[0] + shoulder, inner_bottom_left[1]),
        at((inner_top_left[0] + shoulder, top), top + bar),
        at(outer_bottom_left, top + bar),
        at(outer_bottom_left, top),
        at(stem_left, top),
    ]
    rounded = [(round(x), round(y)) for x, y in outline]

    def draw(pen: AbstractPen) -> None:
        pen.moveTo(rounded[0])
        for point in rounded[1:]:
            pen.lineTo(point)
        pen.closePath()

    return draw


def install(font: TTFont, addition: Addition, cell: int, draw: Draw) -> None:
    bounds = BoundsPen(cast("Mapping[str, Drawable]", font.getGlyphSet()))
    draw(bounds)
    box = cast("tuple[float, float, float, float] | None", bounds.bounds)
    if box is None:
        msg = f"U+{addition.codepoint:04X} has no outline"
        raise ValueError(msg)

    top = cast("CFFTable", cast("object", font["CFF "])).cff.topDictIndex[0]
    pen = T2CharStringPen(cell - top.Private.nominalWidthX, None)
    draw(pen)
    charstring = cast("Callable[[], T2CharString]", pen.getCharString)()
    charstring.private = top.Private
    charstring.globalSubrs = top.GlobalSubrs
    strings = top.CharStrings
    strings.charStrings[addition.glyph] = len(strings.charStringsIndex)
    strings.charStringsIndex.append(charstring)
    order = [*font.getGlyphOrder(), addition.glyph]
    font.setGlyphOrder(order)
    top.charset = order
    cast("Metrics", font["hmtx"]).metrics[addition.glyph] = (cell, round(box[0]))
    for table in cast("Cmaps", font["cmap"]).tables:
        if table.isUnicode():
            table.cmap[addition.codepoint] = addition.glyph


def add_glyphs(font_path: Path) -> None:
    with (
        TTFont(font_path, recalcTimestamp=False) as font,
        TTFont(DONOR_FONT) as variable,
    ):
        cmap = font.getBestCmap()
        if cmap is None:
            msg = "missing character map"
            raise ValueError(msg)
        for addition in (DOUBLE_OBLIQUE_HYPHEN, SHOULDERED_OPEN_BOX):
            if addition.codepoint in cmap or addition.glyph in font.getGlyphOrder():
                msg = f"U+{addition.codepoint:04X} already present"
                raise ValueError(msg)

        cell = cast("Metrics", font["hmtx"]).metrics[cmap[ord(" ")]][0]
        draws = [
            (DOUBLE_OBLIQUE_HYPHEN, double_oblique_hyphen(font, variable, cell)),
            (SHOULDERED_OPEN_BOX, shouldered_open_box(font, cmap)),
        ]
        for addition, draw in draws:
            install(font, addition, cell, draw)
        style = cast("OS2", cast("object", font["OS/2"]))
        _ = style.recalcUnicodeRanges(font)

        donor_copyright = cast("Names", cast("object", variable["name"])).getDebugName(
            0
        )
        if not donor_copyright:
            msg = "Noto Sans Mono donor lacks a copyright notice"
            raise ValueError(msg)
        license_text = (DONOR_DIR / "OFL.txt").read_text().strip()
        names = cast("Names", cast("object", font["name"]))
        additions = {0: donor_copyright, 13: license_text}
        for record in list(names.names):
            if record.nameID in additions:
                names.setName(
                    record.toUnicode() + "\n\n" + additions[record.nameID],
                    record.nameID,
                    record.platformID,
                    record.platEncID,
                    record.langID,
                )
        font.save(font_path)


def main() -> None:
    parser = argparse.ArgumentParser()
    _ = parser.add_argument("fonts", nargs="+", type=Path)
    args = parser.parse_args()
    for font_path in cast("list[Path]", args.fonts):
        try:
            add_glyphs(font_path)
        except ValueError as error:
            error.add_note(str(font_path))
            raise


if __name__ == "__main__":
    main()
