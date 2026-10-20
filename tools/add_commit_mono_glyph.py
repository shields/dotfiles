#!/usr/bin/env python
# pyright: reportMissingTypeStubs=false
# ruff: noqa: CPY001

import argparse
from pathlib import Path
from typing import TYPE_CHECKING, Protocol, cast

from fontTools.misc.transform import Transform
from fontTools.pens.boundsPen import BoundsPen
from fontTools.pens.t2CharStringPen import T2CharStringPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont
from fontTools.varLib import instancer

if TYPE_CHECKING:
    from collections.abc import Callable, Mapping

    from fontTools.misc.psCharStrings import T2CharString
    from fontTools.pens.basePen import AbstractPen

DONOR_DIR = Path(__file__).resolve().parents[1] / "commit-mono" / "noto-sans-mono"
CODEPOINT = 0x2E17
GLYPH = "uni2E17"
ITALIC_SHEAR = 1 / 6


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


def add_glyph(font_path: Path) -> None:
    with (
        TTFont(font_path, recalcTimestamp=False) as font,
        TTFont(DONOR_DIR / "NotoSansMono-DoubleObliqueHyphen.ttf") as variable,
    ):
        cmap = font.getBestCmap()
        if cmap is None or CODEPOINT in cmap or GLYPH in font.getGlyphOrder():
            msg = f"{font_path}: missing character map or U+2E17 already present"
            raise ValueError(msg)

        style = cast("OS2", cast("object", font["OS/2"]))
        instantiate = cast(
            "Callable[[TTFont, dict[str, int]], TTFont]",
            instancer.instantiateVariableFont,
        )
        donor = instantiate(variable, {"wght": style.usWeightClass, "wdth": 100})
        donor_cmap = donor.getBestCmap()
        if donor_cmap is None or CODEPOINT not in donor_cmap:
            msg = "Noto Sans Mono donor lacks U+2E17"
            raise ValueError(msg)
        donor_glyph = donor_cmap[CODEPOINT]
        glyphs = cast("Mapping[str, Drawable]", donor.getGlyphSet())
        cell = cast("Metrics", font["hmtx"]).metrics[cmap[ord(" ")]][0]
        donor_cell = cast("Metrics", donor["hmtx"]).metrics[donor_glyph][0]
        if (
            cast("Head", cast("object", font["head"])).unitsPerEm
            != cast("Head", cast("object", donor["head"])).unitsPerEm
            or cell != donor_cell
        ):
            msg = f"{font_path}: donor and target cell geometry differ"
            raise ValueError(msg)

        transform = Transform()
        if style.fsSelection & 1:
            # The italic metadata is near zero; the outlines use a 1:6 shear.
            transform = Transform(1, 0, ITALIC_SHEAR, 1, -cell * ITALIC_SHEAR / 2, 0)
        bounds = BoundsPen(glyphs)
        glyphs[donor_glyph].draw(TransformPen(bounds, transform))
        box = cast("tuple[float, float, float, float] | None", bounds.bounds)
        if box is None:
            msg = "Noto Sans Mono U+2E17 has no outline"
            raise ValueError(msg)

        top = cast("CFFTable", cast("object", font["CFF "])).cff.topDictIndex[0]
        pen = T2CharStringPen(cell - top.Private.nominalWidthX, None)
        glyphs[donor_glyph].draw(TransformPen(pen, transform))
        charstring = cast("Callable[[], T2CharString]", pen.getCharString)()
        charstring.private = top.Private
        charstring.globalSubrs = top.GlobalSubrs
        strings = top.CharStrings
        strings.charStrings[GLYPH] = len(strings.charStringsIndex)
        strings.charStringsIndex.append(charstring)
        order = [*font.getGlyphOrder(), GLYPH]
        font.setGlyphOrder(order)
        top.charset = order
        cast("Metrics", font["hmtx"]).metrics[GLYPH] = (cell, round(box[0]))
        for table in cast("Cmaps", font["cmap"]).tables:
            if table.isUnicode():
                table.cmap[CODEPOINT] = GLYPH
        _ = style.recalcUnicodeRanges(font)

        donor_copyright = cast("Names", cast("object", donor["name"])).getDebugName(0)
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
        add_glyph(font_path)


if __name__ == "__main__":
    main()
