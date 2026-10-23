# ruff: noqa: CPY001

import json
import os
import shutil
import subprocess
import sys
import time
import unicodedata
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, TypedDict, cast

import pytest

from tests.test_emacs_render import DEFAULT_COLORS, Cell, Terminal

if TYPE_CHECKING:
    from collections.abc import Iterable

REPO = Path(__file__).resolve().parents[1]
SAMPLE = "a b\tc \t \n" + "abcdefghij" * 10 + "\n" + "y" * 35 + "\t\tz\t\n"
FILES = [
    "sample.txt",
    "main.go",
    "go.mod",
    "go.work",
    "Makefile",
    "Makefile.am",
    "data.tsv",
]
VISIBLE_ROW = "a b⇥    c␣⇥     ␣"
HIDDEN_ROW = "a b     c␣      ␣"
CONTROL_VISIBLE_ROW = "a^Ab^?<85>c⇥    d"
CONTROL_HIDDEN_ROW = "a^Ab^?<85>c     d"
BLANK_CELL = 1
# Vim's TSV syntax colors the letters.
LETTER_CELLS = (0, 2, 8)
TAB_CELLS = (*range(3, 8), *range(10, 16))
TRAIL_CELLS = (9, 16)
LEFT_EDGE_TAB_CELLS = (*range(1, 11), *range(12, 19))
CONTROL_CELLS = (1, 2, 4, 5, 6, 7, 8, 9)
CONTROL_TAB_CELLS = range(11, 16)
SEARCHED_CELLS = range(9, 17)
NBSP_FILES = ["nbsp/main.go", "nbsp/Makefile", "nbsp/notes.txt"]
NBSP_TEXT_FILE = "nbsp/notes.txt"
NBSP_MARK = "⍽"
COMBINED_BASES = [
    "e\u0301",
    "\u26a0\ufe0f",
    "\u2764\ufe0f",
    "\u0e01\u0e34\u0e49",
    "\u05d0\u05b0",
    "\u0627\u064e",
]


class NbspRow(NamedTuple):
    line: str
    visible: str
    hidden: str
    gray: tuple[int, ...] = ()
    marked: tuple[int, ...] = ()
    tab_cells: tuple[int, ...] = ()


# Cell numbers count from 0. A tab is marked only where tabs are visible, and
# then every cell of it is.
NBSP_ROWS = [
    NbspRow("a\u00a0b\u202fc", "a⍽b⍽c", "a⍽b⍽c", gray=(1, 3)),
    NbspRow("x\u00a0", "x⍽", "x⍽", marked=(1,)),
    NbspRow("y\u202f", "y⍽", "y⍽", marked=(1,)),
    NbspRow(
        "z \u00a0\t\u202f ",
        "z ⍽⇥    ⍽␣",
        "z ⍽     ⍽␣",
        marked=(1, 2, 8, 9),
        tab_cells=(3, 4, 5, 6, 7),
    ),
    NbspRow("w \u00a0", "w ⍽", "w ⍽", marked=(1, 2)),
    NbspRow("p\u00a0q \u00a0", "p⍽q ⍽", "p⍽q ⍽", gray=(1,), marked=(3, 4)),
    NbspRow("s \u00a0 t", "s ⍽ t", "s ⍽ t", gray=(2,)),
    NbspRow("u\u202f\t", "u⍽⇥", "u⍽", marked=(1,), tab_cells=(2, 3, 4, 5, 6, 7)),
    NbspRow(
        "v\u00a0\tt",
        "v⍽⇥     t",
        "v⍽      t",
        gray=(1,),
        tab_cells=(2, 3, 4, 5, 6, 7),
    ),
    NbspRow("\u00a0\u00a0", "⍽⍽", "⍽⍽", marked=(0, 1)),
    NbspRow(" \u202f", " ⍽", " ⍽", marked=(0, 1)),
    NbspRow(
        "q\u00a0\t\u00a0",
        "q⍽⇥     ⍽",
        "q⍽      ⍽",
        marked=(1, 8),
        tab_cells=(2, 3, 4, 5, 6, 7),
    ),
    *(
        NbspRow(f"{base}  ", f"{base}␣␣", f"{base}␣␣", marked=(1, 2))
        for base in COMBINED_BASES
    ),
    NbspRow("e\u0301\u00a0", "e\u0301⍽", "e\u0301⍽", marked=(1,)),
    NbspRow("\u2764\ufe0f\u202f", "\u2764\ufe0f⍽", "\u2764\ufe0f⍽", marked=(1,)),
    NbspRow("e\u0301\u00a0 ", "e\u0301⍽␣", "e\u0301⍽␣", marked=(1, 2)),
    NbspRow("e\u0301\u00a0x", "e\u0301⍽x", "e\u0301⍽x", gray=(1,)),
]
NBSP_SAMPLE = "".join(row.line + "\n" for row in NBSP_ROWS)


class VimCells(TypedDict):
    text: str
    attrs: list[int]


class VimBufferReport(TypedDict):
    name: str
    rows: list[str]
    attrs: list[int]
    text: list[str]
    width: int


class VimWindowReport(VimCells):
    buffer: str
    filetype: str
    matches: int


class VimStep(TypedDict):
    step: str
    windows: list[VimWindowReport]


class VimTruncation(TypedDict):
    right: VimCells
    left: VimCells


class VimReport(TypedDict):
    marker: int
    light: int
    fresh_matches: int
    reports: list[VimBufferReport]
    hidden: VimCells
    restored: VimCells
    nolist: VimCells
    relisted: VimCells
    searched: VimCells
    steps: list[VimStep]
    resourced: VimCells
    wincolor: VimCells
    controls: dict[str, VimCells]
    truncation: dict[str, VimTruncation]
    nbsp: dict[str, list[VimCells]]
    nbsp_searched: list[VimCells]
    nbsp_nolist: list[VimCells]
    nbsp_backgrounds: dict[str, VimCells]
    colors: list[str]
    nontext_colors: list[str]
    nbsp_colors: list[str]
    filler_background: list[str]
    truecolor: int
    regexpengine: int


VIM_CELLS = r"""
function! Cells(row, first, last) abort
    let cols = range(a:first, a:last)
    return {'text': join(map(copy(cols), {_, col -> screenstring(a:row, col)}), ''),
                \ 'attrs': map(copy(cols), {_, col -> screenattr(a:row, col)})}
endfunction
function! WindowCells(winid, line) abort
    let [row, col] = win_screenpos(a:winid)
    return Cells(row + a:line - 1, col, col + winwidth(a:winid) - 1)
endfunction
"""

VIM_SCRIPT = (
    r"""
set lines=30 columns=40
" Shaping would put the presentation form of an Arabic test base on the screen.
set noarabicshape
execute 'set regexpengine=' . $WHITESPACE_REGEXPENGINE
"""
    + VIM_CELLS
    + r"""
function! Rows(last) abort
    return map(range(1, a:last), {_, row -> Cells(row, 1, 40)})
endfunction
function! Matches(winid) abort
    return len(filter(getmatches(a:winid), {_, m -> m.group =~# '^Shields'}))
endfunction
function! Step(name) abort
    redraw!
    let windows = []
    for nr in range(1, winnr('$'))
        let id = win_getid(nr)
        call add(windows, extend(WindowCells(id, 1),
                    \ {'buffer': fnamemodify(bufname(winbufnr(id)), ':t'),
                    \ 'filetype': getbufvar(winbufnr(id), '&filetype'),
                    \ 'matches': Matches(id)}))
    endfor
    call add(g:steps, {'step': a:name, 'windows': windows})
endfunction
function! Edit(name) abort
    execute 'edit ' . fnameescape($WHITESPACE_DIR . '/' . a:name)
endfunction
function! Colors(name) abort
    let group = synIDtrans(hlID(a:name))
    return [synIDattr(group, 'fg#', 'gui'), synIDattr(group, 'bg#', 'gui'),
                \ synIDattr(group, 'fg', 'cterm'), synIDattr(group, 'bg', 'cterm')]
endfunction
function! Truncation() abort
    set nowrap
    normal! gg2j0
    redraw!
    let right = Cells(3, 33, 40)
    normal! 37zl
    redraw!
    let left = Cells(3, 1, 20)
    set wrap
    normal! gg0
    return {'right': right, 'left': left}
endfunction

" screenattr() numbers exist only within a session, so read ShieldsWhitespace's
" from a cell that a match colors.
new
setlocal buftype=nofile bufhidden=wipe
let fresh_matches = Matches(win_getid())
call setline(1, ['Z', 'Y'])
call matchadd('ShieldsWhitespace', 'Z')
call matchadd('ShieldsNoBreakSpace', 'Y')
redraw!
let marker = screenattr(win_screenpos(0)[0], 1)
let light = screenattr(win_screenpos(0)[0] + 1, 1)
close

let reports = []
for name in json_decode($WHITESPACE_FILES)
    call Edit(name)
    normal! gg0
    redraw!
    let rows = map(range(1, 4), {_, row -> Cells(row, 1, 40).text})
    call add(reports, {'name': name, 'rows': rows,
                \ 'attrs': Cells(1, 1, 40).attrs, 'text': getline(1, '$'),
                \ 'width': strdisplaywidth(getline(1))})
endfor
let b:shields_show_tabs = 0
doautocmd BufWinEnter
redraw!
let hidden = Cells(1, 1, 40)
let b:shields_show_tabs = 1
doautocmd BufWinEnter
redraw!
let restored = Cells(1, 1, 40)

" OptionSet does not fire while Vim is starting, so the script triggers it.
setlocal nolist
doautocmd OptionSet list
redraw!
let nolist = Cells(1, 1, 40)
setlocal list
doautocmd OptionSet list
redraw!
let relisted = Cells(1, 1, 40)
set hlsearch
let @/ = '\s\+$'
redraw!
let searched = Cells(1, 1, 40)
set nohlsearch

let steps = []
split
call Edit('main.go')
call Step('split')
buffer sample.txt
call Step('buffer')
bnext
call Step('bnext')
vsplit
call Step('vsplit')
setlocal filetype=text
call Step('filetype text')
setlocal filetype=go
call Step('filetype go')
wincmd j
let b:shields_show_tabs = 0
doautocmd BufWinEnter
call Step('show_tabs off')
let b:shields_show_tabs = 1
doautocmd BufWinEnter
call Step('show_tabs on')
new
setlocal buftype=nofile bufhidden=wipe
call setline(1, readfile($WHITESPACE_DIR . '/sample.txt')[0])
call Step('new')
setlocal filetype=go
call Step('new go')
setlocal filetype=text
call Step('new text')
only!
call Edit('main.go')
tab split
call Step('tab split')
tabclose
call Edit('sample.txt')
call Step('tab closed')
only!

call Edit('main.go')
source $WHITESPACE_VIMRC
redraw!
let resourced = Cells(1, 1, 40)

let nbsp = {}
for name in json_decode($WHITESPACE_NBSP_FILES)
    call Edit(name)
    redraw!
    let nbsp[name] = Rows(line('$'))
endfor
call Edit('nbsp/notes.txt')
set hlsearch
let @/ = '[\u00a0\u202f]'
redraw!
let nbsp_searched = Rows(line('$'))
set nohlsearch
let nbsp_backgrounds = {}
for background in ['dark', 'light']
    execute 'set background=' . background
    redraw!
    let nbsp_backgrounds[background] = Cells(1, 1, 40)
endfor
setlocal nolist
doautocmd OptionSet list
redraw!
let nbsp_nolist = Rows(line('$'))
call Edit('main.go')

highlight ShieldsTestWindow ctermbg=252 guibg=#cccccc
setlocal wincolor=ShieldsTestWindow
redraw!
let wincolor = Cells(1, 1, 40)
setlocal wincolor=

let controls = {}
for filetype in ['go', 'text']
    new
    setlocal buftype=nofile bufhidden=wipe
    execute 'setlocal filetype=' . filetype
    call setline(1, "a\<C-A>b\x7f\u0085c\td")
    for background in ['dark', 'light']
        execute 'set background=' . background
        redraw!
        let controls[filetype . ' ' . background] = WindowCells(win_getid(), 1)
    endfor
    close
endfor

let truncation = {}
for name in ['sample.txt', 'main.go']
    call Edit(name)
    let truncation[name] = Truncation()
endfor
colorscheme default
let colors = Colors('ShieldsWhitespace')
let nontext_colors = Colors('NonText')
let nbsp_colors = Colors('ShieldsNoBreakSpace')
let filler = synIDtrans(hlID('EndOfBuffer'))
let filler_background = [synIDattr(filler, 'bg#', 'gui'),
            \ synIDattr(filler, 'bg', 'cterm')]
call writefile([json_encode({'marker': marker, 'light': light,
            \ 'fresh_matches': fresh_matches, 'reports': reports,
            \ 'hidden': hidden, 'restored': restored, 'nolist': nolist,
            \ 'relisted': relisted, 'searched': searched, 'steps': steps,
            \ 'resourced': resourced, 'wincolor': wincolor,
            \ 'controls': controls, 'truncation': truncation,
            \ 'nbsp': nbsp, 'nbsp_searched': nbsp_searched,
            \ 'nbsp_nolist': nbsp_nolist, 'nbsp_backgrounds': nbsp_backgrounds,
            \ 'colors': colors, 'nontext_colors': nontext_colors,
            \ 'nbsp_colors': nbsp_colors,
            \ 'filler_background': filler_background,
            \ 'truecolor': &termguicolors,
            \ 'regexpengine': &regexpengine})], $WHITESPACE_REPORT)
qa!
"""
)

TABS_VISIBLE_BY_STEP = {
    "split": [False, True],
    "buffer": [True, True],
    "bnext": [False, True],
    "vsplit": [False, False, True],
    "filetype text": [True, True, True],
    "filetype go": [False, False, True],
    "show_tabs off": [False, False, False],
    "show_tabs on": [False, False, True],
    "new": [False, False, True, True],
    "new go": [False, False, False, True],
    "new text": [False, False, True, True],
    "tab split": [False],
    "tab closed": [True],
}


def screen_cells(text: str) -> list[str]:
    cells: list[str] = []
    for char in text:
        if cells and unicodedata.category(char) in {"Mn", "Me"}:
            cells[-1] += char
        else:
            cells.append(char)
    return cells


def check_cells(
    cells: VimCells,
    row: str,
    *,
    marked: Iterable[int],
    letters: Iterable[int],
    marker: int,
    blank: int,
    gray: Iterable[int] = (),
    light: int = -1,
) -> None:
    assert cells["text"].rstrip() == row, cells
    letter_cells = set(letters)
    marked_cells = set(marked)
    gray_cells = set(gray)
    for index, attr in enumerate(cells["attrs"]):
        if index in letter_cells:
            continue
        if index in marked_cells:
            expected = marker
        elif index in gray_cells:
            expected = light
        else:
            expected = blank
        assert attr == expected, (index, cells)


def check_row(cells: VimCells, *, marker: int, blank: int, tabs_visible: bool) -> None:
    check_cells(
        cells,
        VISIBLE_ROW if tabs_visible else HIDDEN_ROW,
        marked=(*TRAIL_CELLS, *(TAB_CELLS if tabs_visible else ())),
        letters=LETTER_CELLS,
        marker=marker,
        blank=blank,
    )


def check_nbsp_row(
    cells: VimCells,
    spec: NbspRow,
    *,
    marker: int,
    light: int,
    blank: int,
    tabs_visible: bool,
) -> None:
    check_cells(
        cells,
        spec.visible if tabs_visible else spec.hidden,
        marked=(*spec.marked, *(spec.tab_cells if tabs_visible else ())),
        letters=(),
        marker=marker,
        blank=blank,
        gray=spec.gray,
        light=light,
    )


@pytest.mark.parametrize("regexpengine", [0, 1, 2])
@pytest.mark.parametrize("truecolor", [False, True])
def test_vim_whitespace_display(
    tmp_path: Path, *, truecolor: bool, regexpengine: int
) -> None:
    vim = shutil.which("vim")
    assert vim, "vim is required"
    for name in FILES:
        _ = (tmp_path / name).write_text(SAMPLE)
    (tmp_path / "nbsp").mkdir()
    for name in NBSP_FILES:
        _ = (tmp_path / name).write_text(NBSP_SAMPLE, encoding="utf-8")
    script = tmp_path / "check.vim"
    _ = script.write_text(VIM_SCRIPT)
    report_path = tmp_path / "report.json"
    result = subprocess.run(
        [
            vim,
            "-N",
            "-n",
            "-i",
            "NONE",
            "-u",
            str(REPO / ".vimrc"),
            "--not-a-term",
            "-S",
            str(script),
        ],
        env=os.environ
        | {
            "HOME": str(tmp_path),
            "LC_ALL": "C",
            "TERM": "xterm-256color",
            "TERM_PROGRAM": "iTerm.app" if truecolor else "",
            "COLORTERM": "",
            "WHITESPACE_DIR": str(tmp_path),
            "WHITESPACE_FILES": json.dumps([*FILES, "sample.txt"]),
            "WHITESPACE_NBSP_FILES": json.dumps(NBSP_FILES),
            "WHITESPACE_REGEXPENGINE": str(regexpengine),
            "WHITESPACE_REPORT": str(report_path),
            "WHITESPACE_VIMRC": str(REPO / ".vimrc"),
        },
        capture_output=True,
        check=False,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    data = cast("VimReport", json.loads(report_path.read_text()))
    marker = data["marker"]
    light = data["light"]
    blank = data["reports"][0]["attrs"][BLANK_CELL]
    assert len({blank, marker, light}) == 3
    for report in data["reports"]:
        visible = report["name"] in {"sample.txt", "data.tsv"}
        assert report["rows"][2].startswith("⸗")
        assert report["width"] == 17
        assert report["text"] == SAMPLE.splitlines()
        check_row(
            {"text": report["rows"][0], "attrs": report["attrs"]},
            marker=marker,
            blank=blank,
            tabs_visible=visible,
        )
    check_row(data["hidden"], marker=marker, blank=blank, tabs_visible=False)
    check_row(data["restored"], marker=marker, blank=blank, tabs_visible=True)
    assert all(attr == blank for attr in data["nolist"]["attrs"])
    check_row(data["relisted"], marker=marker, blank=blank, tabs_visible=True)
    searched = data["searched"]["attrs"]
    assert all(searched[index] not in {marker, blank} for index in SEARCHED_CELLS)
    assert all(searched[index] == marker for index in range(3, 8))

    assert {step["step"] for step in data["steps"]} == set(TABS_VISIBLE_BY_STEP)
    assert data["fresh_matches"] > 0
    for step in data["steps"]:
        expected = TABS_VISIBLE_BY_STEP[step["step"]]
        assert len(step["windows"]) == len(expected), step
        for window, tabs_visible in zip(step["windows"], expected, strict=True):
            check_row(window, marker=marker, blank=blank, tabs_visible=tabs_visible)
            assert window["matches"] == data["fresh_matches"], step

    check_row(data["resourced"], marker=marker, blank=blank, tabs_visible=False)

    assert set(data["nbsp"]) == set(NBSP_FILES)
    for name, rows in data["nbsp"].items():
        assert len(rows) == len(NBSP_ROWS)
        for cells, spec in zip(rows, NBSP_ROWS, strict=True):
            check_nbsp_row(
                cells,
                spec,
                marker=marker,
                light=light,
                blank=blank,
                tabs_visible=name == NBSP_TEXT_FILE,
            )
    for cells in data["nbsp_backgrounds"].values():
        check_nbsp_row(
            cells,
            NBSP_ROWS[0],
            marker=marker,
            light=light,
            blank=blank,
            tabs_visible=True,
        )
    assert set(data["nbsp_backgrounds"]) == {"dark", "light"}
    assert len(data["nbsp_nolist"]) == len(NBSP_ROWS)
    for cells in data["nbsp_nolist"]:
        assert NBSP_MARK not in cells["text"], cells
        assert all(attr == blank for attr in cells["attrs"]), cells

    found: set[int] = set()
    for cells, spec in zip(data["nbsp_searched"], NBSP_ROWS, strict=True):
        nbsp_cells = {
            i for i, cell in enumerate(screen_cells(spec.visible)) if cell == NBSP_MARK
        }
        check_cells(
            cells,
            spec.visible,
            marked=(*spec.marked, *spec.tab_cells),
            letters=nbsp_cells,
            marker=marker,
            blank=blank,
            gray=spec.gray,
            light=light,
        )
        found.update(cells["attrs"][i] for i in nbsp_cells)
    assert len(found) == 1
    assert found.isdisjoint({blank, marker, light})

    wincolor = data["wincolor"]
    tinted = wincolor["attrs"][BLANK_CELL]
    assert tinted not in {blank, marker}
    assert wincolor["text"].rstrip() == HIDDEN_ROW
    assert all(wincolor["attrs"][index] == tinted for index in TAB_CELLS)
    assert all(wincolor["attrs"][index] == marker for index in TRAIL_CELLS)

    assert set(data["controls"]) == {"go dark", "go light", "text dark", "text light"}
    for key, cells in data["controls"].items():
        visible = key.startswith("text")
        check_cells(
            cells,
            CONTROL_VISIBLE_ROW if visible else CONTROL_HIDDEN_ROW,
            marked=(*CONTROL_CELLS, *(CONTROL_TAB_CELLS if visible else ())),
            letters=(),
            marker=marker,
            blank=blank,
        )

    for name, truncation in data["truncation"].items():
        visible = name == "sample.txt"
        right_tab_cells = range(3, 7) if visible else ()
        check_cells(
            truncation["right"],
            "yyy⇥   …" if visible else "yyy    …",
            marked=(*right_tab_cells, 7),
            letters=range(3),
            marker=marker,
            blank=blank,
        )
        check_cells(
            truncation["left"],
            "…  ⇥" + " " * 7 + "z⇥" if visible else "…" + " " * 10 + "z",
            marked=(0, *(LEFT_EDGE_TAB_CELLS if visible else ())),
            letters=(11,),
            marker=marker,
            blank=blank,
        )

    assert data["colors"] == ["#365773", "#e8f4ff", "24", "195"]
    assert data["nontext_colors"] == data["colors"]
    assert data["nbsp_colors"] == ["#949494", "", "246", ""]
    assert data["filler_background"] == ["", ""]
    assert bool(data["truecolor"]) == truecolor
    assert data["regexpengine"] == regexpengine
    assert all((tmp_path / name).read_text() == SAMPLE for name in FILES)
    assert all(
        (tmp_path / name).read_text(encoding="utf-8") == NBSP_SAMPLE
        for name in NBSP_FILES
    )


TERMINAL_LINES = ["one  ", "z \u00a0\t\u202f ", "three  "]
CURSOR_LINES = ["one \u202f", TERMINAL_LINES[1], "three \t"]
CURSOR_SCRIPT = (
    r"""
set lines=12 columns=40
" An <Esc> at the end of the keys would wait to be told apart from a key code.
set ttimeout ttimeoutlen=0
execute 'set regexpengine=' . $WHITESPACE_REGEXPENGINE
"""
    + VIM_CELLS
    + r"""
function! ClickBottom() abort
    let [row, col] = win_screenpos(win_getid(2))
    call test_setmouse(row, col + 3)
    call feedkeys("\<LeftMouse>", 'nt')
endfunction
function! OpenPrompt() abort
    new
    setlocal buftype=prompt
    call prompt_setprompt(bufnr(), '>> ')
    startinsert
endfunction

new
setlocal buftype=nofile bufhidden=wipe
call setline(1, ['Z', 'Y', 'X', 'W'])
call matchadd('ShieldsWhitespace', 'Z')
call matchadd('ShieldsNoBreakSpace', 'Y')
call matchadd('ShieldsHidden', 'X')
redraw!
let s:top = win_screenpos(0)[0]
let s:attrs = map({'M': 0, 'G': 1, 'H': 2, '.': 3},
            \ {_, offset -> screenattr(s:top + offset, 1)})
close

execute 'edit ' . fnameescape($WHITESPACE_FILE)
let s:plan = [
            \ ['append', 'A'],
            \ ['left', "\<Left>"],
            \ ['end', "\<End>"],
            \ ['down', "\<Down>"],
            \ ['nonblank', 'x'],
            \ ['backspace', "\<BS>"],
            \ ['escape', "\<Esc>"],
            \ ['virtualedit', ":set virtualedit=onemore\<CR>$l"],
            \ ['split', ":split\<CR>"],
            \ ['other window', 'gg'],
            \ ['back', "\<C-w>j"],
            \ ['insert elsewhere', "\<C-w>kotwo  "],
            \ ['edit elsewhere', "\<Esc>"],
            \ ['hlsearch', ":let @/ = '\\s\\+$' | set hlsearch\<CR>"],
            \ ['reset', ":set nohlsearch virtualedit=\<CR>:only\<CR>gg$"],
            \ ['split plain', ":split\<CR>"],
            \ ['delete plain', "A\<BS>"],
            \ ['escape plain', "\<Esc>"],
            \ ['append plain', "A"],
            \ ['click', "\<Cmd>call ClickBottom()\<CR>"],
            \ ['return', "\<Esc>l\<C-w>k"],
            \ ['append escape', "A\<Esc>"],
            \ ['last line', "GA"],
            \ ['last line escape', "\<Esc>"],
            \ ['virtual plain', ":set virtualedit=onemore\<CR>gg$l"],
            \ ['nolist', ":setlocal nolist\<CR>"],
            \ ['nolist move', "j$l"],
            \ ['list', ":setlocal list\<CR>"],
            \ ['scratch', ":set virtualedit=\<CR>:only\<CR>:enew!\<CR>"],
            \ ['prompt', ":call OpenPrompt()\<CR>"],
            \ ['leave prompt', "\<C-w>w"],
            \ ]
let s:steps = []

" SafeState waits for the keys and the autocommands they trigger; a timer can
" run while an <Esc> is still pending.
function! Step() abort
    if exists('s:name')
        redraw
        let windows = []
        for nr in range(1, winnr('$'))
            let id = win_getid(nr)
            call add(windows, map(range(1, line('$', id)),
                        \ {_, lnum -> WindowCells(id, lnum)}))
        endfor
        call add(s:steps, {'step': s:name, 'mode': mode(), 'windows': windows})
    endif
    if empty(s:plan)
        call Finish()
    endif
    let [s:name, keys] = remove(s:plan, 0)
    call feedkeys(keys, 'nt')
endfunction

function! Finish() abort
    let report = {'attrs': s:attrs, 'steps': s:steps, 'codes': [&t_so, &t_se]}
    set term=xterm-256color
    let report.term_changed = [&t_so, &t_se]
    colorscheme default
    let group = synIDtrans(hlID('ShieldsHidden'))
    let report.standout = [synIDattr(group, 'standout', 'cterm'),
                \ synIDattr(group, 'standout', 'gui')]
    set term=vt100
    let report.mono = [&t_Co, &t_so, &t_se]
    call writefile([json_encode(report)], $WHITESPACE_REPORT)
    qa!
endfunction

autocmd SafeState * ++nested call Step()
"""
)

# The classes of the first cells of a line, for each state of the cursor around
# it: M marker colors, G gray, H hidden, S search, . none of these, and T a tab
# cell, which is M where tabs are visible and . where they are blank. The lines
# are CURSOR_LINES, "two  " typed below the first, and "one " after its last
# character is deleted. A window with 'list' off shows no class.
ONE, ONE_AT_END = "...MM", "...HG"
TWO, TWO_AT_END = "...MM", "...HH"
Z, Z_AT_END, Z_TYPED = ".MMTTTTTMM", ".HGTTTTTGH", "..GTTTTTG.."
THREE, THREE_AT_END = ".....MTT", ".....HTT"
ONE_BLANK, ONE_BLANK_AT_END = "...M", "...H"
PROMPT, PROMPT_AT_END = "..M", "..H"
TWO_SEARCHED, THREE_SEARCHED = "...SS", ".....SSS"
Z_SEARCHED, Z_AT_END_SEARCHED = ".MMTTTTTMS", ".HGTTTTTGS"
# For each step, the mode and the lines of each window from the top.
CURSOR_STEPS = {
    "append": ("i", [(ONE_AT_END, Z, THREE)]),
    "left": ("i", [(ONE, Z, THREE)]),
    "end": ("i", [(ONE_AT_END, Z, THREE)]),
    "down": ("i", [(ONE, Z_AT_END, THREE)]),
    "nonblank": ("i", [(ONE, Z_TYPED, THREE)]),
    "backspace": ("i", [(ONE, Z_AT_END, THREE)]),
    "escape": ("n", [(ONE, Z, THREE)]),
    "virtualedit": ("n", [(ONE, Z_AT_END, THREE)]),
    "split": ("n", [(ONE, Z_AT_END, THREE)] * 2),
    "other window": ("n", [(ONE, Z, THREE), (ONE, Z_AT_END, THREE)]),
    "back": ("n", [(ONE, Z, THREE), (ONE, Z_AT_END, THREE)]),
    "insert elsewhere": (
        "i",
        [(ONE, TWO_AT_END, Z, THREE), (ONE, TWO, Z_AT_END, THREE)],
    ),
    "edit elsewhere": ("n", [(ONE, TWO, Z, THREE), (ONE, TWO, Z_AT_END, THREE)]),
    "hlsearch": (
        "n",
        [
            (ONE, TWO_SEARCHED, Z_SEARCHED, THREE_SEARCHED),
            (ONE, TWO_SEARCHED, Z_AT_END_SEARCHED, THREE_SEARCHED),
        ],
    ),
    "reset": ("n", [(ONE, TWO, Z, THREE)]),
    "split plain": ("n", [(ONE, TWO, Z, THREE)] * 2),
    "delete plain": (
        "i",
        [(ONE_BLANK_AT_END, TWO, Z, THREE), (ONE_BLANK, TWO, Z, THREE)],
    ),
    "escape plain": ("n", [(ONE_BLANK, TWO, Z, THREE)] * 2),
    "append plain": (
        "i",
        [(ONE_BLANK_AT_END, TWO, Z, THREE), (ONE_BLANK, TWO, Z, THREE)],
    ),
    "click": ("i", [(ONE_BLANK_AT_END, TWO, Z, THREE), (ONE_BLANK, TWO, Z, THREE)]),
    "return": ("n", [(ONE_BLANK, TWO, Z, THREE)] * 2),
    "append escape": ("n", [(ONE_BLANK, TWO, Z, THREE)] * 2),
    "last line": (
        "i",
        [(ONE_BLANK, TWO, Z, THREE_AT_END), (ONE_BLANK, TWO, Z, THREE)],
    ),
    "last line escape": ("n", [(ONE_BLANK, TWO, Z, THREE)] * 2),
    "virtual plain": (
        "n",
        [(ONE_BLANK_AT_END, TWO, Z, THREE), (ONE_BLANK, TWO, Z, THREE)],
    ),
    "nolist": ("n", [("", "", "", ""), (ONE_BLANK, TWO, Z, THREE)]),
    "nolist move": ("n", [("", "", "", ""), (ONE_BLANK, TWO, Z, THREE)]),
    "list": (
        "n",
        [(ONE_BLANK, TWO_AT_END, Z, THREE), (ONE_BLANK, TWO, Z, THREE)],
    ),
    "scratch": ("n", [("",)]),
    "prompt": ("i", [(PROMPT_AT_END,), ("",)]),
    "leave prompt": ("n", [(PROMPT,), ("",)]),
}
HIDE_CODES = ["\x1b[8m", "\x1b[28m"]


class CursorStep(TypedDict):
    step: str
    mode: str
    windows: list[list[VimCells]]


class CursorReport(TypedDict):
    attrs: dict[str, int]
    steps: list[CursorStep]
    codes: list[str]
    term_changed: list[str]
    standout: list[str]
    mono: list[str]


@pytest.mark.parametrize("regexpengine", [0, 1, 2])
@pytest.mark.parametrize("truecolor", [False, True])
@pytest.mark.parametrize("name", ["cursor.txt", "cursor.go"])
def test_vim_cursor_run(
    tmp_path: Path, *, name: str, truecolor: bool, regexpengine: int
) -> None:
    vim = shutil.which("vim")
    assert vim, "vim is required"
    path = tmp_path / name
    _ = path.write_text("".join(f"{line}\n" for line in CURSOR_LINES))
    script = tmp_path / "check.vim"
    _ = script.write_text(CURSOR_SCRIPT)
    report_path = tmp_path / "report.json"
    # Vim reads standard input between steps and quits at its end, so the pipe
    # stays open until Vim exits.
    keys, keep_open = os.pipe()
    try:
        result = subprocess.run(
            [
                vim,
                "-N",
                "-n",
                "-i",
                "NONE",
                "-u",
                str(REPO / ".vimrc"),
                "--not-a-term",
                "-S",
                str(script),
            ],
            env=os.environ
            | {
                "HOME": str(tmp_path),
                "LC_ALL": "C",
                "TERM": "xterm-256color",
                "TERM_PROGRAM": "iTerm.app" if truecolor else "",
                "COLORTERM": "",
                "WHITESPACE_FILE": str(path),
                "WHITESPACE_REGEXPENGINE": str(regexpengine),
                "WHITESPACE_REPORT": str(report_path),
            },
            stdin=keys,
            capture_output=True,
            check=False,
            timeout=30,
        )
    finally:
        os.close(keys)
        os.close(keep_open)
    assert result.returncode == 0, result.stdout + result.stderr
    data = cast("CursorReport", json.loads(report_path.read_text()))
    classes = {attr: cls for cls, attr in data["attrs"].items()}
    assert len(classes) == len(data["attrs"])
    searched = next(step for step in data["steps"] if step["step"] == "hlsearch")
    search = searched["windows"][0][1]["attrs"][3]
    assert search not in classes
    classes[search] = "S"
    tab = "M" if name.endswith(".txt") else "."
    assert [step["step"] for step in data["steps"]] == list(CURSOR_STEPS)
    for step in data["steps"]:
        mode, windows = CURSOR_STEPS[step["step"]]
        assert step["mode"] == mode, step
        assert len(step["windows"]) == len(windows), step
        for window, rows in zip(step["windows"], windows, strict=True):
            for cells, row in zip(window, rows, strict=True):
                got = "".join(classes.get(attr, "?") for attr in cells["attrs"])
                assert got == row.replace("T", tab).ljust(len(got), "."), (
                    step["step"],
                    cells,
                )
    assert data["codes"] == HIDE_CODES
    assert data["term_changed"] == HIDE_CODES
    assert data["standout"] == ["1", "1"]
    colors, *codes = data["mono"]
    assert int(colors or 0) < 2
    assert codes != HIDE_CODES


@pytest.mark.parametrize("typeahead", [False, True])
@pytest.mark.parametrize("truecolor", [False, True])
def test_vim_hides_trailing_spaces_in_terminal(
    tmp_path: Path, *, truecolor: bool, typeahead: bool
) -> None:
    vim = shutil.which("vim")
    assert vim, "vim is required"
    path = tmp_path / "cursor.txt"
    _ = path.write_text("".join(f"{line}\n" for line in TERMINAL_LINES))
    marker = ("#365773", "#e8f4ff") if truecolor else ("24", "195")
    terminal = Terminal(tmp_path, truecolor=truecolor)
    terminal.pane = "t"

    def wait_for(*, hidden: bool) -> list[list[Cell]]:
        deadline = time.monotonic() + 30
        while True:
            screen = terminal.capture()
            if any(cell.hidden for cell in screen.rows[0]) == hidden:
                return screen.rows
            assert time.monotonic() < deadline, screen.dump()
            time.sleep(0.005)

    try:
        _ = terminal.tmux(
            "new-session",
            "-d",
            "-s",
            "t",
            "-x",
            "40",
            "-y",
            "10",
            "env",
            "-i",
            f"PATH={os.environ['PATH']}",
            f"HOME={tmp_path}",
            "LC_ALL=C.UTF-8",
            "TERM=xterm-256color",
            f"COLORTERM={'truecolor' if truecolor else ''}",
            vim,
            "-N",
            "-n",
            "-i",
            "NONE",
            "-u",
            str(REPO / ".vimrc"),
            "--cmd",
            "set ttimeout ttimeoutlen=0",
            *([] if typeahead else ["+startinsert!"]),
            str(path),
            # A key typed while Vim starts is still pending when Insert mode
            # starts, and then Vim skips CursorMovedI.
            *([";", "send-keys", "-t", "t", "A"] if typeahead else []),
        )
        rows = wait_for(hidden=True)
        assert "".join(cell.char for cell in rows[0][:5]) == "one␣␣", rows[0]
        assert all(cell.hidden for cell in rows[0][3:5]), rows[0]
        assert [cell.colors for cell in rows[0][3:5]] == [DEFAULT_COLORS] * 2, rows[0]
        assert not any(cell.hidden for cell in rows[0][:3]), rows[0]
        assert [cell.colors for cell in rows[2][5:7]] == [marker] * 2, rows[2]
        assert not any(cell.hidden for row in rows[1:] for cell in row), rows
        _ = terminal.tmux("send-keys", "-t", "t", "Escape")
        rows = wait_for(hidden=False)
        assert [cell.colors for cell in rows[0][3:5]] == [marker] * 2, rows[0]
    finally:
        terminal.close()


def test_pager_keeps_input_and_arguments(tmp_path: Path) -> None:
    stub = tmp_path / "less"
    _ = stub.write_text(
        f"""#!{sys.executable}
import json, os, sys
env = os.environ
print(json.dumps([env['LESS'], env['LESSCHARSET'], sys.argv[1:], sys.stdin.read()]))
"""
    )
    stub.chmod(0o755)
    args = ["-R", "-S", "file with spaces.txt"]
    content = SAMPLE + "\x1b[31mcolored\x1b[0m\n"
    result = subprocess.run(
        [str(REPO / "bin/pager"), *args],
        env=os.environ
        | {
            "LESSCHARSET": "latin1",
            "PATH": f"{tmp_path}{os.pathsep}{os.environ['PATH']}",
        },
        input=content,
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    options, charset, forwarded_args, forwarded_input = cast(
        "tuple[str, str, list[str], str]", json.loads(result.stdout)
    )
    assert "--rscroll=…" in options.split()
    assert charset == "utf-8"
    assert "--color=R24.195" in options.split()
    assert "--color=S16.226" in options.split()
    assert forwarded_args == args
    assert forwarded_input == content
