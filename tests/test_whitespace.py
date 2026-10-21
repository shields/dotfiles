# ruff: noqa: CPY001

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import TypedDict, cast

import pytest

REPO = Path(__file__).resolve().parents[1]
SAMPLE = "a\tb \t \n" + "abcdefghij" * 10 + "\n"
FILES = ["sample.txt", "main.go", "go.mod", "go.work", "Makefile", "data.tsv"]


class VimBufferReport(TypedDict):
    name: str
    rows: list[str]
    text: list[str]
    width: int


class VimReport(TypedDict):
    reports: list[VimBufferReport]
    hidden: str
    truncated: str
    colors: list[str]
    filler_background: list[str]
    truecolor: int


VIM_SCRIPT = r"""
set lines=14 columns=40
let reports = []
for name in json_decode($WHITESPACE_FILES)
    execute 'edit ' . fnameescape($WHITESPACE_DIR . '/' . name)
    normal! gg0
    redraw!
    let rows = map(range(1, 4), {_, row ->
                \ join(map(range(1, 40), {_, col -> screenstring(row, col)}), '')})
    call add(reports, {'name': name, 'rows': rows, 'text': getline(1, '$'),
                \ 'width': strdisplaywidth(getline(1))})
endfor
let b:shields_show_tabs = 0
doautocmd BufWinEnter
redraw!
let hidden = join(map(range(1, 40), {_, col -> screenstring(1, col)}), '')
let b:shields_show_tabs = 1
doautocmd BufWinEnter
set nowrap
normal! j0
redraw!
let truncated = screenstring(2, 40)
colorscheme default
let group = synIDtrans(hlID('SpecialKey'))
let colors = [synIDattr(group, 'fg#', 'gui'), synIDattr(group, 'bg#', 'gui'),
            \ synIDattr(group, 'fg', 'cterm'), synIDattr(group, 'bg', 'cterm')]
let filler = synIDtrans(hlID('EndOfBuffer'))
let filler_background = [synIDattr(filler, 'bg#', 'gui'),
            \ synIDattr(filler, 'bg', 'cterm')]
call writefile([json_encode({'reports': reports, 'hidden': hidden,
            \ 'truncated': truncated, 'colors': colors,
            \ 'filler_background': filler_background,
            \ 'truecolor': &termguicolors})], $WHITESPACE_REPORT)
qa!
"""


@pytest.mark.parametrize("truecolor", [False, True])
def test_vim_whitespace_display(tmp_path: Path, *, truecolor: bool) -> None:
    vim = shutil.which("vim")
    assert vim, "vim is required"
    for name in FILES:
        _ = (tmp_path / name).write_text(SAMPLE)
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
            "TERM": "xterm-256color",
            "TERM_PROGRAM": "iTerm.app" if truecolor else "",
            "COLORTERM": "",
            "WHITESPACE_DIR": str(tmp_path),
            "WHITESPACE_FILES": json.dumps([*FILES, "sample.txt"]),
            "WHITESPACE_REPORT": str(report_path),
        },
        capture_output=True,
        check=False,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    data = cast("VimReport", json.loads(report_path.read_text()))
    for report in data["reports"]:
        visible = report["name"] in {"sample.txt", "data.tsv"}
        assert report["rows"][0].rstrip() == (
            "a⇥      b␣⇥     ␣" if visible else "a       b␣      ␣"
        )
        assert report["rows"][2].startswith("⸗")
        assert report["width"] == 17
        assert report["text"] == SAMPLE.splitlines()
    assert "⇥" not in data["hidden"]
    assert data["hidden"].count("␣") == 2
    assert data["truncated"] == "…"
    assert data["colors"] == ["#365773", "#e8f4ff", "24", "195"]
    assert data["filler_background"] == ["", ""]
    assert bool(data["truecolor"]) == truecolor
    assert all((tmp_path / name).read_text() == SAMPLE for name in FILES)


def test_pager_keeps_input_and_arguments(tmp_path: Path) -> None:
    stub = tmp_path / "less"
    _ = stub.write_text(
        f"""#!{sys.executable}
import json, os, sys
print(json.dumps([os.environ['LESS'], sys.argv[1:], sys.stdin.read()]))
"""
    )
    stub.chmod(0o755)
    args = ["-R", "-S", "file with spaces.txt"]
    content = SAMPLE + "\x1b[31mcolored\x1b[0m\n"
    result = subprocess.run(
        [str(REPO / "bin/pager"), *args],
        env=os.environ | {"PATH": f"{tmp_path}{os.pathsep}{os.environ['PATH']}"},
        input=content,
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    assert result.returncode == 0, result.stderr
    options, forwarded_args, forwarded_input = cast(
        "tuple[str, list[str], str]", json.loads(result.stdout)
    )
    assert "--rscroll=…" in options.split()
    assert "--color=R24.195" in options.split()
    assert "--color=S16.226" in options.split()
    assert forwarded_args == args
    assert forwarded_input == content
