# ruff: noqa: CPY001

# The batch tests in test_emacs_tty.py check characters and overlays, which do
# not show where redisplay puts them. These run Emacs with the repo's
# configuration in a real terminal and read the screen, so they see tab stops
# and colors as a user does. tmux is the terminal: test_inputrc_tmux.py already
# needs it, and it keeps a screen that can be captured with its colors. When
# Emacs is idle it redisplays and sets the terminal's title, and tmux handles
# the title after the screen updates before it, so waiting for the title needs
# no sleep. Redisplay skips fontification in a command that starts with input
# pending (init-editing.el sets redisplay-skip-fontification-on-input), so each
# command is sent alone.

import itertools
import json
import os
import re
import shutil
import signal
import subprocess
import tempfile
import time
from dataclasses import dataclass, replace
from typing import TYPE_CHECKING, Self, TypedDict, cast, final

import pytest

from tests.test_emacs_tty import EMACS_DIR, FILES, HARNESS_SETUP

if TYPE_CHECKING:
    from collections.abc import Iterable, Iterator
    from pathlib import Path
    from types import TracebackType

WIDTH = 40
HEIGHT = 14
TIMEOUT = 60.0
POLL = 0.005

DRIVER = r"""
(require 'go-ts-mode)
(require 'make-mode)
;; Mode hooks start spell checkers and language servers, and call packages that
;; are not installed here.  A long message must not resize the window, and a
;; completion preview shows words from the dictionary past the typed ones.
(global-completion-preview-mode -1)
(setq enable-local-variables :all
      text-mode-hook nil
      prog-mode-hook nil
      go-ts-mode-hook nil
      go-mod-ts-mode-hook nil
      go-work-ts-mode-hook nil
      resize-mini-windows nil)

(defvar h-commands 0)
(defvar h-runs 0)

(defun h-report ()
  (redisplay t)
  (send-string-to-terminal
   (format "\e]2;%s\a"
           (json-encode
            `((commands . ,h-commands)
              (runs . ,h-runs)
              (windows
               . ,(vconcat
                   (mapcar (lambda (window)
                             (with-current-buffer (window-buffer window)
                               `((buffer . ,(buffer-name))
                                 (mode . ,(symbol-name major-mode))
                                 (edges . ,(vconcat (window-inside-edges window))))))
                           (window-list nil 'no-minibuf)))))))))

(defun h-visit ()
  (interactive)
  (find-file (with-temp-buffer
               (insert-file-contents (getenv "DOTFILES_RENDER_VISIT"))
               (buffer-string))))

(defun h-quit ()
  (interactive)
  (with-temp-file (expand-file-name "messages" (getenv "HOME"))
    (insert-buffer-substring "*Messages*"))
  (kill-emacs 0))

;; The terminal's answer arrives after this command, as the answer to the query
;; for the terminal's version does when Emacs has stopped waiting for it.
(defun h-query ()
  (interactive)
  (let ((xterm-query-timeout nil))
    (catch 'result
      (xterm--query "\e[>0q"
                    `(("\eP>|" . ,(lambda ()
                                    (throw 'result (xterm--read-string ?\e ?\\))))))
      nil)))

(defun h-state ()
  (interactive)
  (with-temp-file (expand-file-name "state.json" (getenv "HOME"))
    (insert (json-encode
             `((background_mode . ,(symbol-name (frame-parameter nil 'background-mode)))
               (frame_background . ,(frame-parameter nil 'background-color))
               (frame_foreground . ,(frame-parameter nil 'foreground-color))
               (faces . ,(mapcar
                          (lambda (face)
                            (cons face (face-attribute face :foreground nil 'default)))
                          '(link error))))))))

(keymap-global-set "<f8>" #'h-state)
(keymap-global-set "<f9>" #'h-visit)
(keymap-global-set "<f10>" #'h-quit)
(keymap-global-set "<f11>" #'h-query)
;; A terminal reply that arrives late runs the hook without a command.
(add-hook 'post-command-hook
          (lambda ()
            (cl-incf h-runs)
            (when this-command (cl-incf h-commands))))
(add-hook 'emacs-startup-hook (lambda () (run-with-idle-timer 0 t #'h-report)))
"""

SGR = re.compile(r"\x1b\[(?P<sgr>[0-9;:]*)m|(?P<char>.)", re.DOTALL)

POLL_REPLY = re.compile(r"([01]) (\d+) (\d+) (.*)", re.DOTALL)

MARKERS = "⇥␣⸗…"


@dataclass(frozen=True)
class Colors:
    region: str
    marker: tuple[str, str]
    nobreak: str
    space_before_tab: tuple[str, str]
    space_after_tab: tuple[str, str]
    missing_newline: tuple[str, str]


TRUECOLOR = Colors(
    region="#dfecff",
    marker=("#365773", "#e8f4ff"),
    nobreak="#949494",
    space_before_tab=("#b22222", "#ff8c00"),
    space_after_tab=("#b22222", "#cdcd00"),
    missing_newline=("#000000", "#d0d040"),
)
# The theme names these palette colors, which are also Vim's, for 256-color
# terminals.
PALETTE = Colors(
    region="195",
    marker=("24", "195"),
    nobreak="246",
    space_before_tab=("124", "208"),
    space_after_tab=("124", "3"),
    missing_newline=("0", "185"),
)


# Ordinary text carries no color parameter, so the terminal draws it in its own
# foreground and background.
DEFAULT = ""
DEFAULT_COLORS = (DEFAULT, DEFAULT)


@dataclass(frozen=True)
class Cell:
    char: str
    fg: str = DEFAULT
    bg: str = DEFAULT

    @property
    def colors(self) -> tuple[str, str]:
        return (self.fg, self.bg)


def parse_sgr(style: Cell, params: list[int]) -> Cell:
    index = 0
    while index < len(params):
        code = params[index]
        index += 1
        if code == 0:
            style = Cell(" ")
        elif code in {1, 4, 22, 24}:
            continue
        elif 30 <= code <= 37:
            style = replace(style, fg=str(code - 30))
        elif 90 <= code <= 97:
            style = replace(style, fg=str(code - 90 + 8))
        elif code == 39:
            style = replace(style, fg=DEFAULT)
        elif 40 <= code <= 47:
            style = replace(style, bg=str(code - 40))
        elif 100 <= code <= 107:
            style = replace(style, bg=str(code - 100 + 8))
        elif code == 49:
            style = replace(style, bg=DEFAULT)
        elif code in {38, 48}:
            if params[index] == 5:
                color = str(params[index + 1])
                index += 2
            else:
                assert params[index] == 2, params
                red, green, blue = params[index + 1 : index + 4]
                color = f"#{red:02x}{green:02x}{blue:02x}"
                index += 4
            style = replace(style, fg=color) if code == 38 else replace(style, bg=color)
        else:
            message = f"unhandled SGR parameter {code} in {params}"
            raise AssertionError(message)
    return style


def parse_capture(text: str) -> list[list[Cell]]:
    style = Cell(" ")
    rows: list[list[Cell]] = []
    for line in text.removesuffix("\n").split("\n"):
        row: list[Cell] = []
        for match in SGR.finditer(line):
            if match["sgr"] is not None:
                params = [
                    int(part.split(":")[0]) for part in match["sgr"].split(";") if part
                ]
                style = parse_sgr(style, params or [0])
            else:
                assert match["char"] != "\x1b", line
                row.append(replace(style, char=match["char"]))
        assert len(row) <= WIDTH, line
        row.extend(replace(style, char=" ") for _ in range(WIDTH - len(row)))
        rows.append(row)
    return rows


class WindowReport(TypedDict):
    buffer: str
    mode: str
    edges: list[int]


class Report(TypedDict):
    commands: int
    runs: int
    windows: list[WindowReport]


@dataclass(frozen=True)
class Window:
    buffer: str
    mode: str
    left: int
    top: int
    right: int
    bottom: int


@dataclass(frozen=True)
class Screen:
    rows: list[list[Cell]]
    windows: list[Window]
    cursor: tuple[int, int]

    @property
    def window(self) -> Window:
        assert len(self.windows) == 1, self.dump()
        return self.windows[0]

    @property
    def body(self) -> list[list[Cell]]:
        window = self.window
        return [
            row[window.left : window.right]
            for row in self.rows[window.top : window.bottom]
        ]

    @property
    def text(self) -> list[str]:
        return ["".join(cell.char for cell in row).rstrip() for row in self.body]

    def dump(self) -> str:
        lines = ["".join(cell.char for cell in row) for row in self.rows]
        return "\n".join(f"{index:2} |{line}|" for index, line in enumerate(lines))


@final
class Terminal:
    def __init__(
        self, home: Path, *, truecolor: bool, term: str = "xterm-256color"
    ) -> None:
        self.home = home
        self.truecolor = truecolor
        self.term = term
        self.socket_dir = tempfile.mkdtemp(prefix="t")
        self.socket = f"{self.socket_dir}/s"
        self.pane = ""
        self.pid = 0
        self.commands = 0
        self.runs = 0

    def tmux(self, *args: str, check: bool = True) -> str:
        tmux = shutil.which("tmux")
        assert tmux, "tmux is required"
        result = subprocess.run(
            [tmux, "-u", "-S", self.socket, "-f", "/dev/null", *args],
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            encoding="utf-8",
            check=False,
            timeout=TIMEOUT,
        )
        assert not check or result.returncode == 0, (args, result.stderr)
        return result.stdout

    def __enter__(self) -> Self:
        try:
            self.start()
        except BaseException:
            self.close()
            raise
        return self

    def start(self) -> None:
        emacs = shutil.which("emacs")
        assert emacs, "emacs is required"
        driver = self.home / "driver.el"
        _ = driver.write_text(HARNESS_SETUP + DRIVER, encoding="utf-8")
        environment = {
            "PATH": os.environ["PATH"],
            "HOME": str(self.home),
            "LC_ALL": "C.UTF-8",
            "TERM": self.term,
            "COLORTERM": "truecolor" if self.truecolor else "",
            "DOTFILES_EMACS_DIR": str(EMACS_DIR),
            "DOTFILES_EMACS_SCENARIO": "linux",
            "DOTFILES_EMACS_FILES": " ".join(FILES),
            "DOTFILES_RENDER_VISIT": str(self.home / "visit"),
        }
        _ = self.tmux(
            "start-server",
            ";",
            "set-option",
            "-g",
            "remain-on-exit",
            "on",
            ";",
            "new-session",
            "-d",
            "-x",
            str(WIDTH),
            "-y",
            str(HEIGHT),
            "-c",
            str(self.home),
            "env",
            "-i",
            *(f"{name}={value}" for name, value in environment.items()),
            emacs,
            "-nw",
            "-Q",
            "-l",
            str(driver),
        )
        pane, pid = self.tmux("list-panes", "-F", "#{pane_id} #{pane_pid}").split()
        self.pane = pane
        self.pid = int(pid)
        _ = self.wait(None)

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        traceback: TracebackType | None,
    ) -> None:
        try:
            if exc_type is None:
                self.quit()
        finally:
            self.close()

    def close(self) -> None:
        try:
            _ = self.tmux("kill-server", check=False)
        finally:
            try:
                if self.pid:
                    self.reap()
            finally:
                shutil.rmtree(self.socket_dir)

    def poll(self) -> tuple[bool, tuple[int, int], str]:
        deadline = time.monotonic() + TIMEOUT
        while True:
            state = self.tmux(
                "display-message",
                "-p",
                "-t",
                self.pane,
                "#{pane_dead} #{cursor_x} #{cursor_y} #{pane_title}",
            )
            match = POLL_REPLY.fullmatch(state.removesuffix("\n"))
            if match:
                dead, x, y, title = match.groups()
                return dead == "1", (int(x), int(y)), title
            assert time.monotonic() < deadline, repr(state)
            time.sleep(POLL)

    def quit(self) -> None:
        _ = self.tmux("send-keys", "-t", self.pane, "F10")
        deadline = time.monotonic() + TIMEOUT
        while not self.poll()[0]:
            assert time.monotonic() < deadline, self.capture().dump()
            time.sleep(POLL)
        status = self.tmux(
            "display-message", "-p", "-t", self.pane, "#{pane_dead_status}"
        )
        assert status == "0\n", status

    def reap(self) -> None:
        deadline = time.monotonic() + TIMEOUT
        while True:
            try:
                os.kill(self.pid, 0)
            except ProcessLookupError:
                return
            if time.monotonic() > deadline:
                os.kill(self.pid, signal.SIGKILL)
                message = f"Emacs {self.pid} outlived its terminal"
                raise AssertionError(message)
            time.sleep(POLL)

    def capture(self) -> Screen:
        text = self.tmux("capture-pane", "-p", "-e", "-N", "-t", self.pane)
        return Screen(parse_capture(text), [], (0, 0))

    def wait(self, commands: int | None, *, runs: int | None = None) -> Screen:
        deadline = time.monotonic() + TIMEOUT
        while True:
            dead, cursor, title = self.poll()
            assert not dead, self.capture().dump()
            if title.startswith("{"):
                report = cast("Report", json.loads(title))
                assert commands is None or report["commands"] <= commands, (
                    f"Emacs ran {report['commands']} commands, "
                    f"not the {commands} that were sent"
                )
                if commands in {None, report["commands"]} and runs in {
                    None,
                    report["runs"],
                }:
                    break
            assert time.monotonic() < deadline, self.capture().dump()
            time.sleep(POLL)
        self.commands = report["commands"]
        self.runs = report["runs"]
        windows = [
            Window(window["buffer"], window["mode"], *window["edges"])
            for window in report["windows"]
        ]
        return replace(self.capture(), windows=windows, cursor=cursor)

    def send(self, *keys: str) -> Screen:
        _ = self.tmux("send-keys", "-t", self.pane, *keys)
        return self.wait(self.commands + 1)

    def type_text(self, text: str) -> Screen:
        screens: list[Screen] = []
        for char in text:
            _ = self.tmux("send-keys", "-t", self.pane, "-l", "--", char)
            screens.append(self.wait(self.commands + 1))
        return screens[-1]

    def visit(self, path: Path) -> Screen:
        _ = (self.home / "visit").write_text(str(path), encoding="utf-8")
        return self.send("F9")


SAMPLE = "a b\tc\t\t \nx\t\n" + "abcdefghij" * 10 + "\n" + "y" * 35 + "\t\tz\t\n"
VISIBLE_ROWS = [
    "a b⇥    c⇥      ⇥       ␣",
    "x⇥",
    "abcdefghijabcdefghijabcdefghijabcdefghi⸗",
    "jabcdefghijabcdefghijabcdefghijabcdefgh⸗",
    "ijabcdefghijabcdefghij",
    "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy⇥   ⸗",
    " ⇥       z⇥",
]
HIDDEN_ROWS = [
    "a b     c               ␣",
    "x",
    VISIBLE_ROWS[2],
    VISIBLE_ROWS[3],
    VISIBLE_ROWS[4],
    "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy    ⸗",
    "         z",
]
VISIBLE_TRUNCATED_ROWS = [
    VISIBLE_ROWS[0],
    VISIBLE_ROWS[1],
    "abcdefghijabcdefghijabcdefghijabcdefghi…",
    "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy⇥   …",
]
HIDDEN_TRUNCATED_ROWS = [
    HIDDEN_ROWS[0],
    HIDDEN_ROWS[1],
    VISIBLE_TRUNCATED_ROWS[2],
    "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy    …",
]
TAB_SPANS = {
    0: [(3, 8), (9, 16), (16, 24)],
    1: [(1, 8)],
    5: [(35, 39)],
    6: [(0, 9), (10, 17)],
}
TRUNCATED_TAB_SPANS = {0: TAB_SPANS[0], 1: TAB_SPANS[1], 3: [(35, 39)]}

VISIBLE_FORMATS = ("sample.txt", "data.tsv", "Makefile.common")
HIDDEN_FORMATS = (
    "main.go",
    "go.mod",
    "go.work",
    "Makefile",
    "GNUmakefile",
)
HIDDEN_MODES = {
    "main.go": "go-ts-mode",
    "go.mod": "go-mod-ts-mode",
    "go.work": "go-work-ts-mode",
    "Makefile": "makefile-",
    "GNUmakefile": "makefile-",
}
TRUNCATED_FORMATS = ("sample.txt", "main.go")

TAB_LINES = ["abcdefg"[: 8 - width] + "\tX\n" for width in range(8, 0, -1)]
TAB_WIDTH_4 = "-*- tab-width: 4 -*-\n" + "".join(
    "abc"[: 4 - width] + "\tX\n" for width in range(4, 0, -1)
)

EXEMPTION = "one \ntwo  \nthree\n"
# After "C-n" the cursor is in the middle of the blanks on the second line, not
# at their end.
EXEMPTION_STEPS = {
    "start": (["one␣", "two␣␣", "three", ""], (0, 0)),
    "C-e": (["one", "two␣␣", "three", ""], (4, 0)),
    "C-n": (["one␣", "two␣␣", "three", ""], (4, 1)),
    "C-e again": (["one␣", "two", "three", ""], (5, 1)),
    "C-p": (["one", "two␣␣", "three", ""], (4, 0)),
    "C-a": (["one␣", "two␣␣", "three", ""], (0, 0)),
    "M->": (["one␣", "two␣␣", "three", ""], (0, 3)),
    "word": (["one␣", "two␣␣", "three", "word"], (4, 3)),
    "word space": (["one␣", "two␣␣", "three", "word"], (5, 3)),
    "word space x": (["one␣", "two␣␣", "three", "word x"], (6, 3)),
    "DEL": (["one␣", "two␣␣", "three", "word"], (5, 3)),
    "C-p again": (["one␣", "two␣␣", "three", "word␣"], (5, 2)),
}

NOBREAK = (
    "a\u00a0b\u202fc\n"
    "d\u00a0e \u00a0\n"
    "g \u202f\t\u00a0 \n"
    "h\u00a0\u00a0i\n"
    "j\u00a0\n"
    "k\u2011l\u00adm\n"
)
NOBREAK_ROWS = [
    "a⍽b⍽c",
    "d⍽e␣⍽",
    "g␣⍽⇥    ⍽␣",
    "h⍽⍽i",
    "j⍽",
    "k\u2011l\u00adm",
]
NOBREAK_MARKED = [(1, 3), (1, 4), *((2, column) for column in range(1, 10)), (4, 1)]
NOBREAK_GRAY = [(0, 1), (0, 3), (1, 1), (3, 1), (3, 2)]
# The non-breaking hyphen and the soft hyphen on the last line have faces of
# their own.
NOBREAK_HYPHENS = (1, 3)

MIXED = "one \u00a0 \ntwo\n"
MIXED_STEPS = {
    "start": (["one␣⍽␣", "two", ""], (0, 0), "marked"),
    "C-e": (["one ⍽", "two", ""], (6, 0), "exempt"),
    "C-b": (["one␣⍽␣", "two", ""], (5, 0), "marked"),
    "C-n": (["one␣⍽␣", "two", ""], (3, 1), "marked"),
    "C-p": (["one␣⍽␣", "two", ""], (5, 0), "marked"),
    "C-e again": (["one ⍽", "two", ""], (6, 0), "exempt"),
}

NIL_STYLE = "-*- whitespace-style: nil -*-\na\u00a0b \u00a0\n"
LOW_PALETTE = "a\u00a0b \u00a0\n"
# The no-break space's color in terminals without the xterm 256-color palette:
# 16 colors, 8 colors, and terminfo entries from ncurses-term that claim 256
# colors but have only the 8 that Emacs registers.
LOW_PALETTE_GRAYS = {
    "xterm": "6",
    "xterm-16color": "8",
    "linux-16color": "6",
    "putty-256color": "6",
    "vte-256color": "6",
}

SELECTION = "x  \ny\t \nend\n"
MAKEFILE_HIGHLIGHTS = "  \ta\n\t        b\nc"
TEXT_HIGHLIGHTS = "x \ty\n\t        z\nend"


@dataclass(frozen=True)
class Capture:
    colors: Colors
    tab_stops: Screen
    tab_stops_4: Screen
    hidden_tab_stops: Screen
    formats: dict[str, Screen]
    truncated: dict[str, Screen]
    exemption: dict[str, Screen]
    makefile_highlights: Screen
    text_highlights: Screen
    selection: Screen
    nobreak: Screen
    mixed: dict[str, Screen]
    nil_style: Screen
    messages: str


def capture_screens(home: Path, *, truecolor: bool) -> Capture:
    files = home / "files"

    def write(group: str, name: str, text: str) -> Path:
        path = files / group / name
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(text, encoding="utf-8")
        return path

    with Terminal(home, truecolor=truecolor) as term:
        tab_stops = term.visit(write("tabs", "tabs.txt", "".join(TAB_LINES)))
        tab_stops_4 = term.visit(write("tabs", "tabs-4.txt", TAB_WIDTH_4))
        hidden_tab_stops = term.visit(write("tabs", "main.go", "".join(TAB_LINES)))

        formats: dict[str, Screen] = {}
        truncated: dict[str, Screen] = {}
        for name in (*VISIBLE_FORMATS, *HIDDEN_FORMATS):
            formats[name] = term.visit(write("formats", name, SAMPLE))
            if name in TRUNCATED_FORMATS:
                truncated[name] = term.send("C-x", "x", "t")

        exemption = {"start": term.visit(write("exemption", "e.txt", EXEMPTION))}
        exemption["C-e"] = term.send("C-e")
        exemption["C-n"] = term.send("C-n")
        exemption["C-e again"] = term.send("C-e")
        exemption["C-p"] = term.send("C-p")
        exemption["C-a"] = term.send("C-a")
        exemption["M->"] = term.send("M->")
        exemption["word"] = term.type_text("word")
        exemption["word space"] = term.type_text(" ")
        exemption["word space x"] = term.type_text("x")
        exemption["DEL"] = term.send("BSpace")
        exemption["C-p again"] = term.send("C-p")

        makefile_highlights = term.visit(
            write("highlights", "Makefile", MAKEFILE_HIGHLIGHTS)
        )
        text_highlights = term.visit(write("highlights", "notes.txt", TEXT_HIGHLIGHTS))
        _ = term.visit(write("selection", "s.txt", SELECTION))
        _ = term.send("C-Space")
        _ = term.send("C-n")
        _ = term.send("C-n")
        selection = term.send("C-e")

        nobreak = term.visit(write("nobreak", "nobreak.txt", NOBREAK))
        mixed = {"start": term.visit(write("nobreak", "mixed.txt", MIXED))}
        for step in ("C-e", "C-b", "C-n", "C-p"):
            mixed[step] = term.send(step)
        mixed["C-e again"] = term.send("C-e")
        nil_style = term.visit(write("nobreak", "nil.txt", NIL_STYLE))
    return Capture(
        TRUECOLOR if truecolor else PALETTE,
        tab_stops,
        tab_stops_4,
        hidden_tab_stops,
        formats,
        truncated,
        exemption,
        makefile_highlights,
        text_highlights,
        selection,
        nobreak,
        mixed,
        nil_style,
        (home / "messages").read_text(encoding="utf-8"),
    )


@pytest.fixture(scope="module", params=[True, False], ids=["truecolor", "256-color"])
def capture(
    request: pytest.FixtureRequest, tmp_path_factory: pytest.TempPathFactory
) -> Capture:
    truecolor = cast("bool", request.param)
    home = tmp_path_factory.mktemp("truecolor" if truecolor else "palette")
    return capture_screens(home, truecolor=truecolor)


def check_tab_row(
    screen: Screen,
    row: int,
    spans: list[tuple[int, int]],
    colors: Colors,
    *,
    visible: bool,
) -> None:
    covered = {column for start, end in spans for column in range(start, end)}
    for column, cell in enumerate(screen.body[row]):
        if cell.char in MARKERS or (visible and column in covered):
            assert cell.colors == colors.marker, (row, column, screen.dump())
        else:
            assert cell.colors == DEFAULT_COLORS, (row, column, screen.dump())
            assert cell.char == " " or column not in covered, (row, column)


def check_colors(
    screen: Screen, expected: dict[tuple[int, int], tuple[str, str]]
) -> None:
    for row, cells in enumerate(screen.body):
        for column, cell in enumerate(cells):
            if (row, column) in expected:
                assert cell.colors == expected[row, column], (
                    row,
                    column,
                    screen.dump(),
                )
            else:
                assert cell.colors == DEFAULT_COLORS, (row, column, screen.dump())


def test_tab_stops_are_kept_except_by_a_one_column_tab(capture: Capture) -> None:
    screen = capture.tab_stops
    assert screen.window.mode == "text-mode"
    # The mark is ⇥ followed by the tab itself. With one column to the stop, ⇥
    # fills it and the tab that follows goes on to the next stop.
    assert screen.text[:8] == [
        "⇥       X",
        "a⇥      X",
        "ab⇥     X",
        "abc⇥    X",
        "abcd⇥   X",
        "abcde⇥  X",
        "abcdef⇥ X",
        "abcdefg⇥        X",
    ], screen.dump()


def test_the_shift_after_a_one_column_tab_is_the_tab_width(capture: Capture) -> None:
    screen = capture.tab_stops_4
    assert screen.window.mode == "text-mode"
    assert screen.text[:5] == [
        "-*- tab-width: 4 -*-",
        "⇥   X",
        "a⇥  X",
        "ab⇥ X",
        "abc⇥    X",
    ], screen.dump()


def test_marked_tabs_fill_to_the_next_stop_in_the_marker_colors(
    capture: Capture,
) -> None:
    screen = capture.tab_stops
    for row, width in enumerate(range(8, 0, -1)):
        start = 8 - width
        spans = [(start, 16 if width == 1 else 8)]
        check_tab_row(screen, row, spans, capture.colors, visible=True)


def test_hidden_tabs_do_not_shift_text(capture: Capture) -> None:
    screen = capture.hidden_tab_stops
    assert screen.window.mode == "go-ts-mode"
    assert screen.text[:8] == [
        "        X",
        "a       X",
        "ab      X",
        "abc     X",
        "abcd    X",
        "abcde   X",
        "abcdef  X",
        "abcdefg X",
    ], screen.dump()
    for row in range(8):
        check_tab_row(screen, row, [], capture.colors, visible=False)


@pytest.mark.parametrize("name", VISIBLE_FORMATS)
def test_tabs_and_trailing_spaces_are_marked_in_other_formats(
    capture: Capture, name: str
) -> None:
    screen = capture.formats[name]
    assert screen.text == [*VISIBLE_ROWS, *[""] * 5], screen.dump()
    for row in range(len(VISIBLE_ROWS)):
        check_tab_row(screen, row, TAB_SPANS.get(row, []), capture.colors, visible=True)


@pytest.mark.parametrize("name", HIDDEN_FORMATS)
def test_tabs_are_blank_and_trailing_spaces_marked_in_tab_formats(
    capture: Capture, name: str
) -> None:
    screen = capture.formats[name]
    assert screen.window.mode.startswith(HIDDEN_MODES[name])
    assert screen.text == [*HIDDEN_ROWS, *[""] * 5], screen.dump()
    for row in range(len(HIDDEN_ROWS)):
        check_tab_row(
            screen, row, TAB_SPANS.get(row, []), capture.colors, visible=False
        )


@pytest.mark.parametrize("name", TRUNCATED_FORMATS)
def test_truncated_lines_end_in_the_marker(capture: Capture, name: str) -> None:
    screen = capture.truncated[name]
    visible = name in VISIBLE_FORMATS
    rows = VISIBLE_TRUNCATED_ROWS if visible else HIDDEN_TRUNCATED_ROWS
    assert screen.text == [*rows, *[""] * 8], screen.dump()
    for row in range(len(rows)):
        check_tab_row(
            screen,
            row,
            TRUNCATED_TAB_SPANS.get(row, []),
            capture.colors,
            visible=visible,
        )


def test_trailing_spaces_at_point_are_unmarked_until_point_leaves(
    capture: Capture,
) -> None:
    observed = {
        label: (screen.text[:4], screen.cursor)
        for label, screen in capture.exemption.items()
    }
    assert observed == EXEMPTION_STEPS
    for screen in capture.exemption.values():
        assert screen.window.mode == "text-mode"
        for row in range(4):
            check_tab_row(screen, row, [], capture.colors, visible=True)


def test_misplaced_whitespace_is_highlighted_in_makefiles(capture: Capture) -> None:
    screen = capture.makefile_highlights
    colors = capture.colors
    assert screen.window.mode.startswith("makefile-")
    assert screen.text[:4] == ["        a", "                b", "c", ""]
    check_colors(
        screen,
        {
            (0, 0): colors.space_before_tab,
            (0, 1): colors.space_before_tab,
            **{(1, column): colors.space_after_tab for column in range(8, 16)},
            (2, 0): colors.missing_newline,
        },
    )


def test_misplaced_whitespace_is_highlighted_where_tabs_are_marked(
    capture: Capture,
) -> None:
    screen = capture.text_highlights
    colors = capture.colors
    assert screen.window.mode == "text-mode"
    assert screen.text[:4] == ["x ⇥     y", "⇥               z", "end", ""]
    check_colors(
        screen,
        {
            **{(0, column): colors.space_before_tab for column in range(2, 8)},
            **{(1, column): colors.space_after_tab for column in range(8)},
            (2, 2): colors.missing_newline,
        },
    )


def test_a_selection_shows_through_trailing_space_markers(capture: Capture) -> None:
    screen = capture.selection
    markers = [cell for row in screen.body for cell in row if cell.char == "␣"]
    assert [cell.colors for cell in markers] == [
        (capture.colors.marker[0], capture.colors.region)
    ] * 3, screen.dump()


def test_makefile_looking_names_are_not_makefiles(capture: Capture) -> None:
    screen = capture.formats["Makefile.common"]
    assert screen.window.mode == "fundamental-mode"
    assert screen.text == [*VISIBLE_ROWS, *[""] * 5], screen.dump()


def test_256_color_terminals_get_the_palette_colors_vim_uses(
    capture: Capture,
) -> None:
    screen = capture.formats["sample.txt"]
    expected = ("#365773", "#e8f4ff") if capture.colors is TRUECOLOR else ("24", "195")
    cells = [cell for row in screen.body for cell in row if cell.char in MARKERS]
    assert cells
    assert {cell.colors for cell in cells} == {expected}, screen.dump()


def test_every_color_the_faces_name_exists(capture: Capture) -> None:
    assert "Unable to load color" not in capture.messages


def test_text_is_drawn_in_the_terminals_own_colors(capture: Capture) -> None:
    screen = capture.formats["sample.txt"]
    text = [cell for row in screen.body for cell in row if cell.char.isalnum()]
    assert text
    assert {cell.colors for cell in text} == {DEFAULT_COLORS}, screen.dump()
    blank = [cell for row in screen.body[len(VISIBLE_ROWS) :] for cell in row]
    assert blank
    assert {cell.colors for cell in blank} == {DEFAULT_COLORS}, screen.dump()
    mode_line = screen.rows[screen.window.bottom]
    assert all(cell.bg != DEFAULT for cell in mode_line), screen.dump()


def test_no_break_spaces_are_gray_boxes_except_when_trailing(
    capture: Capture,
) -> None:
    screen = capture.nobreak
    colors = capture.colors
    assert screen.window.mode == "text-mode"
    assert screen.text[: len(NOBREAK_ROWS)] == NOBREAK_ROWS, screen.dump()
    gray = (colors.nobreak, DEFAULT)
    hyphens = {screen.body[5][column].colors for column in NOBREAK_HYPHENS}
    assert len(hyphens) == 1, screen.dump()
    (hyphen,) = hyphens
    assert hyphen[0] not in {DEFAULT, colors.nobreak}
    check_colors(
        screen,
        {
            **dict.fromkeys(NOBREAK_GRAY, gray),
            **dict.fromkeys(NOBREAK_MARKED, colors.marker),
            **dict.fromkeys([(5, column) for column in NOBREAK_HYPHENS], hyphen),
        },
    )


def test_a_trailing_run_ending_at_point_is_unmarked_but_keeps_its_box(
    capture: Capture,
) -> None:
    colors = capture.colors
    observed = {
        label: (screen.text[:3], screen.cursor)
        for label, screen in capture.mixed.items()
    }
    assert observed == {
        label: (text, cursor) for label, (text, cursor, _) in MIXED_STEPS.items()
    }
    for label, screen in capture.mixed.items():
        if MIXED_STEPS[label][2] == "marked":
            expected = dict.fromkeys([(0, 3), (0, 4), (0, 5)], colors.marker)
        else:
            expected = {(0, 4): (colors.nobreak, DEFAULT)}
        check_colors(screen, expected)


def test_an_empty_style_leaves_only_the_gray_boxes(capture: Capture) -> None:
    screen = capture.nil_style
    colors = capture.colors
    assert screen.text[:3] == ["-*- whitespace-style: nil -*-", "a⍽b ⍽", ""]
    check_colors(
        screen,
        dict.fromkeys([(1, 1), (1, 4)], (colors.nobreak, DEFAULT)),
    )


@pytest.fixture(scope="module", params=LOW_PALETTE_GRAYS)
def low_palette(
    request: pytest.FixtureRequest, tmp_path_factory: pytest.TempPathFactory
) -> tuple[str, Screen, str]:
    term_name = cast("str", request.param)
    home = tmp_path_factory.mktemp(term_name)
    path = home / "files" / "low.txt"
    path.parent.mkdir()
    _ = path.write_text(LOW_PALETTE, encoding="utf-8")
    with Terminal(home, truecolor=False, term=term_name) as term:
        screen = term.visit(path)
    return term_name, screen, (home / "messages").read_text(encoding="utf-8")


def test_terminals_without_the_palette_still_show_readable_markers(
    low_palette: tuple[str, Screen, str],
) -> None:
    term_name, screen, messages = low_palette
    assert "Unable to load color" not in messages
    assert screen.text[0] == "a⍽b␣⍽", screen.dump()
    box, marker, trailing = screen.body[0][1], screen.body[0][3], screen.body[0][4]
    assert box.bg == DEFAULT
    assert box.fg == LOW_PALETTE_GRAYS[term_name]
    assert marker.fg not in {DEFAULT, marker.bg}
    assert marker.bg != DEFAULT
    assert trailing.colors == marker.colors
    assert box.fg != marker.fg


# TERM values that Emacs treats as light (xterm) or dark (tmux, screen) when the
# default face names no background.
BACKGROUND_TERMS = ("xterm-256color", "tmux-256color", "screen-256color")


@pytest.mark.parametrize("truecolor", [True, False], ids=["truecolor", "256-color"])
@pytest.mark.parametrize("term_name", BACKGROUND_TERMS)
def test_faces_are_light_whatever_the_terminal_type(
    tmp_path: Path, term_name: str, *, truecolor: bool
) -> None:
    with Terminal(tmp_path, truecolor=truecolor, term=term_name) as term:
        _ = term.send("F8")
    state = cast(
        "dict[str, object]",
        json.loads((tmp_path / "state.json").read_text(encoding="utf-8")),
    )
    # The theme gives neither face a color, so these are Emacs's own.
    assert state == {
        "background_mode": "light",
        "frame_background": "unspecified-bg",
        "frame_foreground": "unspecified-fg",
        "faces": {"link": "RoyalBlue3", "error": "Red1"},
    }


@pytest.fixture
def terminal(tmp_path: Path) -> Iterator[Terminal]:
    term = Terminal(tmp_path, truecolor=False)
    yield term
    shutil.rmtree(term.socket_dir)


@final
class TmuxReplies:
    def __init__(self, replies: Iterable[str]) -> None:
        self.replies = iter(replies)

    def __call__(self, *_args: str, **_kwargs: bool) -> str:
        return next(self.replies)


def test_a_terminal_reply_is_not_a_command(tmp_path: Path) -> None:
    with Terminal(tmp_path, truecolor=False) as term:
        commands, runs = term.commands, term.runs
        _ = term.tmux("send-keys", "-t", term.pane, "F11")
        _ = term.wait(None, runs=runs + 2)
        assert term.commands == commands + 1


def test_more_commands_than_were_sent_fail_at_once(
    monkeypatch: pytest.MonkeyPatch, terminal: Terminal
) -> None:
    title = '{"commands":5,"runs":5,"windows":[]}'
    monkeypatch.setattr(terminal, "poll", lambda: (False, (0, 0), title))
    with pytest.raises(AssertionError, match="ran 5 commands, not the 3 that"):
        _ = terminal.wait(3)


def test_a_short_reply_from_tmux_is_polled_again(
    monkeypatch: pytest.MonkeyPatch, terminal: Terminal
) -> None:
    replies = TmuxReplies(["0 \n", "0 0 \n", "   \n", "1 3 4 a title\n"])
    monkeypatch.setattr(terminal, "tmux", replies)
    assert terminal.poll() == (True, (3, 4), "a title")


def test_a_reply_that_stays_short_fails_with_its_text(
    monkeypatch: pytest.MonkeyPatch, terminal: Terminal
) -> None:
    monkeypatch.setitem(globals(), "TIMEOUT", 0.05)
    monkeypatch.setattr(terminal, "tmux", TmuxReplies(itertools.repeat("0 0 \n")))
    with pytest.raises(AssertionError, match=r"'0 0 \\n'"):
        _ = terminal.poll()
