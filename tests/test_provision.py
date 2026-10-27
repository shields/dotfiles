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

# provision.sh changes the machine it runs on, so it is never executed here.
# Its structure is checked from the text, and the copy step's `git ls-files`
# and `tar` run against the repository and a scratch repository: they only
# read. The ~/.claude.json update runs too, against a scratch home.

import json
import os
import re
import runpy
import shlex
import shutil
import stat
import subprocess
from dataclasses import dataclass
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
PROVISION = REPO / "provision.sh"
MACOS = REPO / "provision" / "macos.sh"
SCRIPTS = (
    PROVISION,
    MACOS,
    REPO / "provision" / "modules.sh",
    REPO / "provision" / "linux-system.sh",
)
SCRIPT_IDS = [script.name for script in SCRIPTS]
TEXT = PROVISION.read_text()
LINES = TEXT.splitlines()

MACOS_GUARD = "if [[ $os == macos ]]; then"
LINUX_GUARD = "if [[ $os == linux ]]; then"
MACOS_FUNCTIONS = (
    "macos_vm_access_cleanup",
    "macos_emudhra_workaround",
    "macos_preflight",
    "macos_vm_access_install",
    "macos_xcode",
    "macos_login_shell",
    "macos_defaults",
    "macos_emacs_app",
    "macos_finish",
)
MACOS_ONLY_COMMANDS = (
    "softwareupdate",
    "arch",
    "defaults",
    "dscl",
    "osascript",
    "killall",
    "desktoppr",
    "xcrun",
    "xcodebuild",
    "xcode-select",
    "systemsetup",
    "launchctl",
    "cupertino",
    "profiles",
)

# Paths that are tracked or planned and must not reach a Linux home.
LINUX_EXCLUDES = (
    ".CFUserTextEncoding",
    ".iTerm2/com.googlecode.iterm2.plist",
    ".config/karabiner",
    ".config/ghostty",
    ".config/alacritty",
    ".config/emacs-plus",
    ".gnupg/gpg-agent.conf",
    "bin/chrome-tabs-to-markdown",
    "bin/*.applescript",
    "bin/clean_downloads.py",
    "bin/limavm",
    "bin/vm-access",
    ".agents/skills/transcribe",
    ".claude/skills/transcribe",
)
MACOS_EXCLUDES = (
    "bin/setup-secrets",
    "bin/github_app_token.py",
    "bin/setup-github-gateway",
    "bin/github-gateway-credential",
)
CHEZMOI_EXCLUDES = (
    ".chezmoi*",
    ".gitconfig",
    ".config/bat/config",
    ".config/git/ignore",
    ".config/yt-dlp/config",
)
BASE_COPY_PATHS = [
    ".*",
    "bin",
    ":(exclude).github",
    *(f":(exclude){p}" for p in CHEZMOI_EXCLUDES),
]

COPY_COMMAND = (
    'git ls-files -z -- "${copy_paths[@]}"'
    " | tar --null -cf - -T - | "
    '(cd "$HOME" && tar xvf -)'
)

# The oldest bash provision.sh must run under is macOS's 3.2.
BASH_4_FEATURES = {
    "mapfile": r"\bmapfile\b",
    "readarray": r"\breadarray\b",
    "associative arrays": r"\b(declare|local|typeset)\s+-[a-zA-Z]*A\b",
    "case modification": r"\$\{[^}]*(,,|\^\^)[^}]*\}",
    "[[ -v": r"\[\[\s+-v\b",
    "&>>": r"&>>",
    "|&": r"\|&",
    "case fallthrough": r";;&|;&",
    "coproc": r"\bcoproc\b",
    "shopt features": r"shopt\s+-s\s+(globstar|lastpipe|inherit_errexit)",
    "wait -n": r"\bwait\s+-n\b",
}


def strip_quotes(line: str) -> str:
    return re.sub(r"\"[^\"]*\"|'[^']*'", '""', line)


def code_lines() -> list[tuple[int, str]]:
    return [
        (i, line)
        for i, line in enumerate(LINES)
        if line.strip() and not line.strip().startswith("#")
    ]


def indent_of(line: str) -> int:
    return len(line) - len(line.lstrip(" "))


def openers(index: int) -> list[str]:
    """The block-opening lines that enclose LINES[index], innermost first."""
    indent = indent_of(LINES[index])
    found: list[str] = []
    for line in reversed(LINES[:index]):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if indent_of(line) < indent:
            found.append(stripped)
            indent = indent_of(line)
            if indent == 0:
                break
    return found


def find(pattern: str, start: int = 0) -> int:
    for i in range(start, len(LINES)):
        if re.search(pattern, LINES[i]):
            return i
    msg = f"no line matches {pattern!r} from line {start + 1}"
    raise AssertionError(msg)


def block(header_index: int) -> list[str]:
    """The lines of the if-block whose `if` line is LINES[header_index]."""
    indent = indent_of(LINES[header_index])
    body: list[str] = []
    for line in LINES[header_index + 1 :]:
        if line.strip() == "fi" and indent_of(line) == indent:
            return body
        body.append(line.strip())
    msg = "unterminated block"
    raise AssertionError(msg)


def array_items(text: str, opener: str) -> list[str]:
    start = text.index(opener) + len(opener)
    in_quote = False
    for i in range(start, len(text)):
        if text[i] == "'":
            in_quote = not in_quote
        elif text[i] == ")" and not in_quote:
            return shlex.split(text[start:i])
    msg = f"unterminated array after {opener!r}"
    raise AssertionError(msg)


def copy_pathspecs(os_name: str) -> list[str]:
    base = array_items(TEXT, "copy_paths=(")
    branch_start = TEXT.index(f"\n{os_name})\n", TEXT.index("case $os in"))
    branch = TEXT[branch_start : TEXT.index("\n    ;;", branch_start)]
    return [*base, *array_items(branch, "copy_paths+=(")]


def git_ls_files(
    repo: Path, pathspecs: list[str], env: dict[str, str] | None = None
) -> list[str]:
    if env is None:
        # A git hook exports GIT_DIR and GIT_INDEX_FILE, which would make this
        # list the files of the commit that is being made, not those of repo.
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    result = subprocess.run(
        ["git", "ls-files", "-z", "--", *pathspecs],
        cwd=repo,
        env=env,
        capture_output=True,
        timeout=30,
        check=True,
    )
    return [name for name in result.stdout.decode().split("\0") if name]


def selected(path: str, os_name: str) -> bool:
    """Which tracked paths each OS copies, written independently of the pathspecs."""
    if path == ".github" or path.startswith(".github/"):
        return False
    if path.startswith(".chezmoi") or path in CHEZMOI_EXCLUDES:
        return False
    if os_name == "macos":
        in_scope = path.startswith((".", "bin/", "Library/"))
        return in_scope and path not in MACOS_EXCLUDES
    if not path.startswith((".", "bin/")):
        return False
    excluded = (
        path in {".CFUserTextEncoding", ".gnupg/gpg-agent.conf"}
        or path == ".iTerm2/com.googlecode.iterm2.plist"
        or path.startswith(
            (
                ".config/karabiner/",
                ".config/ghostty/",
                ".config/alacritty/",
                ".config/emacs-plus/",
                ".agents/skills/transcribe/",
                ".claude/skills/transcribe/",
            )
        )
        or path
        in {
            "bin/chrome-tabs-to-markdown",
            "bin/clean_downloads.py",
            "bin/limavm",
            "bin/vm-access",
            ".claude/skills/transcribe",
        }
        or (path.startswith("bin/") and path.endswith(".applescript"))
    )
    return not excluded


@pytest.mark.parametrize("script", SCRIPTS, ids=SCRIPT_IDS)
def test_scripts_parse_under_the_oldest_bash(script: Path) -> None:
    result = subprocess.run(
        ["/bin/bash", "-n", str(script)],
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )
    assert result.returncode == 0, result.stderr


@pytest.mark.parametrize("script", SCRIPTS, ids=SCRIPT_IDS)
def test_scripts_avoid_bash_4_features(script: Path) -> None:
    text = "\n".join(
        line for line in script.read_text().splitlines() if not line.startswith("#")
    )
    found = [
        name for name, pattern in BASH_4_FEATURES.items() if re.search(pattern, text)
    ]
    assert found == []


@pytest.mark.parametrize("script", SCRIPTS, ids=SCRIPT_IDS)
def test_only_the_nonempty_copy_array_is_expanded_whole(script: Path) -> None:
    # bash 3.2 treats an empty array as unset under `set -u`.
    names = set(re.findall(r"\$\{(\w+)\[@\]\}", script.read_text()))
    assert names <= {"copy_paths"}


def test_header_finds_the_checkout_and_the_user_once() -> None:
    header = [line for _, line in code_lines()][:6]
    assert header[:3] == [
        "set -euo pipefail",
        "unset CDPATH",
        'cd "$(dirname "$0")"',
    ]
    assert header[3] == "dotfiles_root=$PWD"
    assert header[4] == "user=$(id -un)"
    assert "$USER" not in TEXT
    assert "BASH_SOURCE" not in TEXT


def test_os_is_detected_once_and_others_are_refused() -> None:
    assert len(re.findall(r"\buname -s\b", TEXT)) == 1
    detect = find(r"^uname_s=\$\(uname -s\)")
    assert LINES[detect + 2 : detect + 4] == [
        "Darwin) os=macos ;;",
        "Linux) os=linux ;;",
    ]
    refusal = (
        '    echo "provision.sh: unsupported OS $uname_s; '
        'only macOS and Linux are supported" >&2'
    )
    assert LINES[detect + 4 : detect + 8] == ["*)", refusal, "    exit 1", "    ;;"]


def test_linux_refuses_to_run_as_root() -> None:
    guard = find(r"os == linux && \$\(id -u\) -eq 0")
    assert "exit 1" in block(guard)


def test_macos_functions_are_defined_in_macos_sh() -> None:
    lines = MACOS.read_text().splitlines()
    assert lines[0] == "# shellcheck shell=bash"
    defined = [line[:-4] for line in lines if line.endswith("() {")]
    assert defined == list(MACOS_FUNCTIONS)
    toplevel = [
        line
        for line in lines
        if line and not line.startswith(("#", " ", "}")) and not line.endswith("() {")
    ]
    assert toplevel == []


def test_macos_functions_are_called_as_plain_statements_in_the_guard() -> None:
    for name in MACOS_FUNCTIONS:
        uses = [i for i, line in enumerate(LINES) if re.search(rf"\b{name}\b", line)]
        assert len(uses) == (2 if name == "macos_emudhra_workaround" else 1), name
        for use in uses:
            assert LINES[use].strip() == name
            assert openers(use)[:1] == [MACOS_GUARD]


def test_macos_functions_are_called_in_the_original_order() -> None:
    calls = [find(rf"^\s+{name}$") for name in MACOS_FUNCTIONS]
    assert calls == sorted(calls)
    (
        cleanup,
        emudhra,
        preflight,
        install,
        xcode,
        login_shell,
        defaults,
        emacs_app,
        finish,
    ) = calls
    analytics = find(r"^brew analytics off")
    oh_my_zsh = find(r"Oh My Zsh installation")
    assert cleanup < emudhra < find(r"^# Install Homebrew") < analytics
    assert analytics < preflight < oh_my_zsh < install < xcode
    assert find(r"^\s+bin/docker-prune$") < xcode
    assert login_shell == xcode + 1
    assert find(r"^\s+macos_emudhra_workaround$", emudhra + 1) == login_shell + 1
    assert find(r"gcloud --quiet components update") < defaults
    emacs = find(r"^emacs --batch --script \.emacs\.d/provision\.el")
    assert defaults < emacs < emacs_app
    assert emacs_app < find(r"Bootstrap TLS trust")
    assert find(r"configure_codex\.py") < finish
    assert [line.strip() for line in LINES[finish + 1 :] if line.strip()] == ["fi"]


def test_macos_sh_is_only_sourced_on_macos() -> None:
    sourced = find(r"^\s+source provision/macos\.sh$")
    assert openers(sourced)[:1] == [MACOS_GUARD]


def test_macos_only_commands_stay_inside_the_macos_guard() -> None:
    word = "|".join(re.escape(c) for c in MACOS_ONLY_COMMANDS)
    lead = r"(?:^|[;&|(!]|\bthen|\bdo|\bif|\belif|\bwhile|\buntil)"
    command = re.compile(rf"{lead}\s*(?:sudo\s+(?:-\S+\s+)*)?({word})(?![\w-])")
    bare: list[str] = []
    for index, line in code_lines():
        match = command.search(strip_quotes(line.strip()))
        if match and MACOS_GUARD not in openers(index):
            bare.append(line.strip())
    assert bare == []


def test_macos_sh_holds_the_macos_only_commands() -> None:
    text = MACOS.read_text()
    for command in set(MACOS_ONLY_COMMANDS) - {"profiles"}:
        assert re.search(rf"\b{command}\b", text), command


def test_chezmoi_applies_after_its_package_is_installed() -> None:
    apply = find(r'^if ! chezmoi --source "\$dotfiles_root"')
    assert LINES[apply].endswith(" --less-interactive --error-on-conflict apply; then")
    assert find(r"^brew autoremove$") < apply < find(r"^    macos_vm_access_install$")
    assert "git config --global user.email" not in TEXT
    assert "git config --global github.user" not in TEXT


def test_copy_uses_git_ls_files_through_tar() -> None:
    assert COPY_COMMAND in TEXT
    assert array_items(TEXT, "copy_paths=(") == BASE_COPY_PATHS
    assert "tar cf - -T - bin Library" not in TEXT


def test_macos_copy_adds_library_and_leaves_out_the_guest_only_scripts() -> None:
    assert copy_pathspecs("macos") == [
        *BASE_COPY_PATHS,
        "Library",
        *(f":(exclude){p}" for p in MACOS_EXCLUDES),
    ]


def test_linux_copy_leaves_out_exactly_the_macos_only_files() -> None:
    assert copy_pathspecs("linux") == [
        *BASE_COPY_PATHS,
        *(f":(exclude){p}" for p in LINUX_EXCLUDES),
    ]


@pytest.mark.parametrize("os_name", ["macos", "linux"])
def test_copy_selects_the_expected_tracked_files(os_name: str) -> None:
    tracked = git_ls_files(REPO, [])
    expected = {path for path in tracked if selected(path, os_name)}
    actual = set(git_ls_files(REPO, copy_pathspecs(os_name)))
    assert actual == expected
    assert ".zshrc" in actual
    assert ".claude/settings.json" in actual


def test_linux_copy_has_no_macos_files_and_macos_copy_has_library() -> None:
    linux = git_ls_files(REPO, copy_pathspecs("linux"))
    macos = git_ls_files(REPO, copy_pathspecs("macos"))
    assert not any(path.startswith("Library/") for path in linux)
    assert any(path.startswith("Library/Fonts/") for path in macos)
    assert set(linux) - set(macos) <= set(MACOS_EXCLUDES)


SCRATCH_FILES = {
    ".chezmoiroot": "home\n",
    ".gitconfig": "",
    ".config/bat/config": "",
    ".config/git/ignore": "",
    ".config/yt-dlp/config": "",
    "home/dot_gitconfig.tmpl": "",
    "home/dot_config/bat/config": "",
    ".zshrc": "",
    ".CFUserTextEncoding": "",
    ".iTerm2/com.googlecode.iterm2.plist": "",
    ".iTerm2/shell_integration.zsh": "",
    ".config/starship.toml": "",
    ".config/karabiner/karabiner.json": "",
    ".config/ghostty/config": "",
    ".config/alacritty/alacritty.toml": "",
    ".config/emacs-plus/build.yml": "",
    ".gnupg/gpg-agent.conf": "",
    ".github/renovate.json5": "",
    ".github/workflows/ci.yml": "",
    ".agents/skills/bughunt/SKILL.md": "",
    ".agents/skills/transcribe/SKILL.md": "",
    "bin/ghfork": "",
    "bin/$": "",
    "bin/chrome-tabs-to-markdown": "",
    "bin/clean_downloads.py": "",
    "bin/limavm": "",
    "bin/vm-access": "",
    "bin/setup-secrets": "",
    "bin/github_app_token.py": "",
    "bin/setup-github-gateway": "",
    "bin/github-gateway-credential": "",
    "bin/zoom-toggle-audio.applescript": "",
    "Library/Fonts/f.otf": "",
    "Library/Application Support/with space.txt": "",
    "README.md": "",
    "tools/stage.sh": "",
}


@pytest.fixture
def scratch_repo(tmp_path: Path) -> tuple[Path, dict[str, str]]:
    repo = tmp_path / "repo"
    for name, content in SCRATCH_FILES.items():
        path = repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(content)
    skills = repo / ".claude" / "skills"
    skills.mkdir(parents=True)
    (skills / "bughunt").symlink_to("../../.agents/skills/bughunt")
    (skills / "transcribe").symlink_to("../../.agents/skills/transcribe")
    # Untracked files must never be copied.
    _ = (repo / "bin" / "scratch-untracked").write_text("")
    env = {
        "PATH": os.environ["PATH"],
        "HOME": str(tmp_path),
        "GIT_CONFIG_GLOBAL": os.devnull,
        "GIT_CONFIG_NOSYSTEM": "1",
    }
    git = shutil.which("git")
    assert git
    _ = subprocess.run([git, "init", "-q"], cwd=repo, env=env, check=True)
    tracked = [
        *SCRATCH_FILES,
        ".claude/skills/bughunt",
        ".claude/skills/transcribe",
    ]
    _ = subprocess.run([git, "add", "--", *tracked], cwd=repo, env=env, check=True)
    return repo, env


def test_scratch_repo_macos_copy_matches_the_independent_selection(
    scratch_repo: tuple[Path, dict[str, str]],
) -> None:
    repo, env = scratch_repo
    tracked = git_ls_files(repo, [], env)
    actual = set(git_ls_files(repo, copy_pathspecs("macos"), env))
    assert actual == {path for path in tracked if selected(path, "macos")}
    assert "bin/setup-secrets" not in actual
    assert "bin/github_app_token.py" not in actual
    assert "bin/setup-github-gateway" not in actual
    assert "bin/github-gateway-credential" not in actual
    assert "bin/scratch-untracked" not in actual
    assert ".github/renovate.json5" not in actual
    assert {"bin/$", "Library/Application Support/with space.txt"} <= actual
    assert ".config/karabiner/karabiner.json" in actual


def test_scratch_repo_linux_copy_drops_every_excluded_path(
    scratch_repo: tuple[Path, dict[str, str]],
) -> None:
    repo, env = scratch_repo
    actual = set(git_ls_files(repo, copy_pathspecs("linux"), env))
    assert actual == {
        ".zshrc",
        ".iTerm2/shell_integration.zsh",
        ".config/starship.toml",
        ".agents/skills/bughunt/SKILL.md",
        ".claude/skills/bughunt",
        "bin/ghfork",
        "bin/$",
        "bin/setup-secrets",
        "bin/github_app_token.py",
        "bin/setup-github-gateway",
        "bin/github-gateway-credential",
    }


def test_scratch_repo_tar_pipeline_carries_unusual_names(
    scratch_repo: tuple[Path, dict[str, str]],
) -> None:
    repo, env = scratch_repo
    names = subprocess.run(
        ["git", "ls-files", "-z", "--", *copy_pathspecs("macos")],
        cwd=repo,
        env=env,
        capture_output=True,
        timeout=30,
        check=True,
    ).stdout
    archive = subprocess.run(
        ["tar", "--null", "-cf", "-", "-T", "-"],
        cwd=repo,
        input=names,
        capture_output=True,
        timeout=30,
        check=True,
    ).stdout
    listing = subprocess.run(
        ["tar", "-tf", "-"],
        input=archive,
        capture_output=True,
        timeout=30,
        check=True,
    ).stdout.decode()
    members = {line for line in listing.splitlines() if not line.endswith("/")}
    assert members == set(git_ls_files(repo, copy_pathspecs("macos"), env))


def test_selection_is_resolved_and_saved_before_anything_changes() -> None:
    resolve = find(r"^modules_resolve ")
    args = [
        '"$os"',
        '"$dotfiles_root/brew"',
        '"$selection_file"',
        '"$have_brew"',
        '"$@"',
    ]
    assert LINES[resolve] == f"modules_resolve {' '.join(args)} || modules_status=$?"
    refuse = find(r"^if \[\[ \$modules_status -ne 0 \]\]; then$")
    assert resolve < refuse < find(r"^modules_persist ")
    assert block(refuse) == ['exit "$modules_status"']
    persist = find(r'^modules_persist "\$selection_file" "\$modules_selection"')
    export = find(
        r"^export HOMEBREW_DOTFILES_BREW_MODULES=\$\{modules_selection:-none\}"
    )
    assert resolve < persist < export < find(r"^git ls-files")
    assert export < find(r"brew shellenv")
    assert "selection_file=$HOME/.config/dotfiles/brew-modules" in LINES
    assert ".config/dotfiles/brew-modules" in (REPO / "Brewfile").read_text()


def test_brew_detection_feeds_the_drop_guard_from_every_prefix() -> None:
    init = find(r"^have_brew=0$")
    loop = find(r"^for brew_candidate in ", init)
    header = LINES[loop].removeprefix("for brew_candidate in ").removesuffix("; do")
    assert set(shlex.split(header)) == {
        "/opt/homebrew/bin/brew",
        "/usr/local/bin/brew",
        "/home/linuxbrew/.linuxbrew/bin/brew",
    }
    found = find(r"^\s+if \[\[ -x \$brew_candidate \]\]; then$", loop)
    assert block(found) == ["have_brew=1", "brew_found=$brew_candidate", "break"]
    assert loop < find(r"^modules_resolve ")


def test_dropped_modules_preview_what_brew_would_uninstall() -> None:
    guard = find(r"modules_status -eq 3 && \$have_brew -eq 1")
    body = "\n".join(block(guard))
    assert (
        'HOMEBREW_DOTFILES_BREW_MODULES=${modules_selection:-none} "$brew_found"'
        in body
    )
    assert "bundle cleanup" in body
    assert "--force" not in body
    assert ">&2 </dev/null || true" in body


def test_homebrew_is_installed_per_os() -> None:
    linux = find(rf"^{re.escape(LINUX_GUARD)}", find(r"Install Homebrew \(which"))
    body, _, macos = "\n".join(block(linux)).partition("\nelse\n")
    body = body.splitlines()
    assert 'HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew"' in body
    assert "if [[ ! -x $HOMEBREW_PREFIX/bin/brew ]]; then" in body
    assert any(line.startswith("NONINTERACTIVE=1 /bin/bash -c") for line in body)
    assert not any("CI=1" in line for line in body)
    assert 'UNAME_MACHINE="$(/usr/bin/uname -m)"' in macos
    assert "CI=1 /bin/bash -c" in macos
    assert "NONINTERACTIVE" not in macos


def test_linux_system_script_runs_with_sudo_before_homebrew() -> None:
    call = find(r'^\s+sudo "\$dotfiles_root/provision/linux-system\.sh" "\$user"$')
    assert openers(call)[:1] == [LINUX_GUARD]
    assert call < find(r"Install Homebrew \(which")


def test_rustup_is_ready_before_bundle_on_dev_only() -> None:
    guard = find(r'^if modules_has "\$modules_selection" dev; then')
    body = block(guard)
    assert body == [
        "brew install --yes rustup",
        "rustup_prefix=$(brew --prefix rustup)",
        'export PATH="$rustup_prefix/bin:$PATH"',
        "rustup default stable >/dev/null",
    ]
    assert guard < find(r"^brew bundle --force --no-upgrade")
    assert sum("rustup default stable" in line for line in LINES) == 1


def test_bundle_steps_and_comment() -> None:
    assert "brew bundle dump" not in TEXT
    assert "Edit brew/*.Brewfile" in TEXT
    bundle = find(r"^brew bundle --force --no-upgrade --upgrade-formulae=")
    assert LINES[bundle + 1] == "brew bundle cleanup --force"
    assert "brew upgrade --formula" not in TEXT
    assert find(r"^uv cache prune") > bundle
    assert LINES[find(r"^go clean -modcache")] == "go clean -modcache"
    assert find(r"^\s+bin/docker-prune$") > find(r"^go clean -modcache")


def provision_uv_cache(home: Path, environment: dict[str, str]) -> str:
    header = find(r"-ef /Volumes/Data/src")
    snippet = "\n".join([LINES[header], *block(header), "fi"])
    volume = shlex.quote(str(home / "Data"))
    result = subprocess.run(
        [
            "bash",
            "-c",
            "set -euo pipefail\n"
            + snippet.replace("/Volumes/Data", volume)
            + '\nprintf %s "${UV_CACHE_DIR-unset}"',
        ],
        env={"PATH": os.environ["PATH"], "HOME": str(home), **environment},
        capture_output=True,
        text=True,
        timeout=15,
        check=True,
    )
    assert result.stderr == ""
    return result.stdout


@pytest.mark.parametrize("linked", [True, False])
def test_provision_selects_the_uv_cache_that_zshenv_does(
    tmp_path: Path, *, linked: bool
) -> None:
    (tmp_path / "Data/src").mkdir(parents=True)
    if linked:
        (tmp_path / "src").symlink_to(tmp_path / "Data/src")
    expected = f"{tmp_path}/Data/uv-cache.noindex" if linked else "unset"
    assert provision_uv_cache(tmp_path, {}) == expected
    assert provision_uv_cache(tmp_path, {"UV_CACHE_DIR": "/x"}) == "/x"


def test_provision_selects_the_uv_cache_before_using_uv() -> None:
    header = find(r"-ef /Volumes/Data/src")
    assert header < find(r"^uv cache prune")
    assert not [line for line in LINES[:header] if re.match(r"\s*uv ", line)]


def test_homebrew_supports_selective_formula_upgrades() -> None:
    result = subprocess.run(
        [
            "brew",
            "bundle",
            "install",
            "--no-upgrade",
            "--upgrade-formulae=git",
            "--help",
        ],
        env=os.environ | {"HOMEBREW_NO_AUTO_UPDATE": "1"},
        capture_output=True,
        text=True,
        check=False,
        timeout=30,
    )
    assert result.returncode == 0, result.stderr
    assert "--upgrade-formulae" in result.stdout


@pytest.mark.parametrize(
    "scenario", ["success", "no_casks", "list_failure", "bundle_failure"]
)
def test_bundle_upgrades_formulae_and_preserves_cask_policy(
    tmp_path: Path, scenario: str
) -> None:
    stub = tmp_path / "brew"
    _ = stub.write_text(
        r"""#!/bin/bash
# Copyright © 2026 Michael Shields
# SPDX-License-Identifier: Apache-2.0
set -euo pipefail
printf '%s\n' "$*" >>"$LOG"
case "$*" in
    'bundle list --formula')
        [[ $SCENARIO != list_failure ]] || exit 7
        printf '%s\n' git shields/tap/mute-on-lock
        ;;
    'bundle --force --no-upgrade --upgrade-formulae='*)
        expected='git,shields/tap/mute-on-lock,mute-on-lock'
        [[ $4 == "--upgrade-formulae=$expected" ]] || exit 64
        [[ $SCENARIO != bundle_failure ]] || exit 8
        printf '%s\n' 'Using git' 'Upgrading mute-on-lock'
        ;;
    'bundle cleanup --force' | autoremove) ;;
    'outdated --greedy-auto-updates --cask --quiet')
        if [[ $SCENARIO != no_casks ]]; then
            printf '%s\n' google-chrome ghostty
        fi
        ;;
    'upgrade --cask --yes ghostty') ;;
    *) exit 64 ;;
esac
"""
    )
    stub.chmod(0o755)
    log = tmp_path / "calls"
    start = find(r"^brew_formulae=")
    stop = find(r"^brew autoremove$", start) + 1
    script = "set -euo pipefail\n" + "\n".join(LINES[start:stop]) + "\necho finished\n"
    result = subprocess.run(
        ["/bin/bash", "-c", script],
        env={
            "PATH": f"{tmp_path}:/usr/bin:/bin",
            "SCENARIO": scenario,
            "LOG": str(log),
        },
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    calls = log.read_text().splitlines()
    if scenario == "list_failure":
        assert result.returncode == 7
        assert calls == ["bundle list --formula"]
    elif scenario == "bundle_failure":
        assert result.returncode == 8
        assert len(calls) == 2
        assert "finished" not in result.stdout
    else:
        assert result.returncode == 0, result.stderr
        assert "Upgrading mute-on-lock" in result.stdout
        assert "Using git" not in result.stdout
        assert calls[-1] == "autoremove"
        assert ("upgrade --cask --yes ghostty" in calls) == (scenario == "success")
        assert not any(
            call.startswith("upgrade") and "google-chrome" in call for call in calls
        )


def test_the_image_build_keeps_homebrew_downloads_and_other_runs_prune_them() -> None:
    cleanup = find(r"^\s+brew cleanup --prune=all$")
    otherwise = LINES[cleanup - 1]
    assert otherwise == "else"
    guard = find(r"^if \[\[ -n \$\{DOTFILES_KEEP_BREW_DOWNLOADS-\} \]\]; then$")
    assert guard < cleanup
    assert [line.strip() for line in LINES[guard + 1 : cleanup - 1]] == ["brew cleanup"]
    dockerfile = (REPO / "cloudflare" / "Dockerfile").read_text()
    assert "DOTFILES_KEEP_BREW_DOWNLOADS=1 ./provision.sh $MODULES" in dockerfile


def test_docker_prune_runs_only_on_macos() -> None:
    prune = find(r"^\s+bin/docker-prune$")
    assert openers(prune)[:1] == [MACOS_GUARD]
    assert sum("bin/docker-prune" in line for _, line in code_lines()) == 1


def test_cloudflare_reconfigures_codex_after_installing_the_throwaway_policy() -> None:
    dockerfile = (REPO / "cloudflare/Dockerfile").read_text()
    guard = dockerfile.index('RUN if [ "$CLOUDFLARE_AGENT" = 1 ]; then')
    throwaway = dockerfile.index("sudo provision/throwaway.sh", guard)
    configure = dockerfile.index("python tools/configure_codex.py", throwaway)
    reset = dockerfile.index("provision/reset-identity.sh", configure)
    end = dockerfile.index("\n    fi", guard)
    assert guard < throwaway < configure < reset < end
    assert (
        'PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"' in dockerfile[throwaway:configure]
    )


def test_the_plugin_tools_come_from_the_modules_that_update_them() -> None:
    data = (REPO / "brew" / "data.Brewfile").read_text()
    assert 'brew "datasette"' in data
    assert 'brew "llm"' in data
    assert 'cask "gcloud-cli"' in (REPO / "brew" / "cloud.Brewfile").read_text()


def test_plugins_update_only_for_the_modules_that_install_them() -> None:
    data = find(r'^if modules_has "\$modules_selection" data; then')
    assert [line.split()[0] for line in block(data)] == ["datasette", "llm"]
    cloud = find(r'^if modules_has "\$modules_selection" cloud; then')
    assert block(cloud) == ["gcloud --quiet components update"]
    assert data < cloud
    assert find(r"^export PIP_DISABLE_PIP_VERSION_CHECK=1") < data


def test_emacs_is_provisioned_on_both_systems() -> None:
    emacs = find(r"^emacs --batch --script \.emacs\.d/provision\.el")
    assert not openers(emacs)


def test_lgtmcp_is_found_in_home_bin_then_gobin() -> None:
    assert "lgtmcp=$HOME/bin/lgtmcp" in LINES
    home_bin = find(r"^lgtmcp=\$HOME/bin/lgtmcp$")
    body = "\n".join(LINES[home_bin : find(r"^add_user_mcp_server lgtmcp ")])
    assert body.index("go env GOBIN") < body.index("go env GOPATH")
    homebrew_env = "GOBIN=${HOMEBREW_GOBIN-} GOPATH=${HOMEBREW_GOPATH-}"
    assert f"gobin=$({homebrew_env} go env GOBIN)" in body
    assert f"gopath=$({homebrew_env} go env GOPATH)" in body
    assert "gobin=${gopath%%:*}/bin" in body
    assert "exit 1" in body
    assert 'add_user_mcp_server lgtmcp "$lgtmcp" -tools review_and_commit' in LINES
    assert '$HOME/bin/lgtmcp"' not in "\n".join(
        LINES[find(r"^add_user_mcp_server lgtmcp ") :]
    )


ROOT_PATH = (
    "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$HOMEBREW_PREFIX/bin"
)


def test_playwright_is_registered_per_os() -> None:
    macos = find(
        r"add_user_mcp_server playwright npx @playwright/mcp@latest --headless"
    )
    assert openers(macos)[:1] == [MACOS_GUARD]
    version = find(r"playwright_version=\$\(npm view @playwright/mcp version\)")
    otherwise = find(r"^else$", macos)
    assert otherwise < version
    steps = [line.strip() for line in LINES[version : find(r"^fi$", version)]]
    pinned = '"@playwright/mcp@$playwright_version"'
    root_npx = (
        f'sudo env DEBIAN_FRONTEND=noninteractive PATH="{ROOT_PATH}" npx -y -p {pinned}'
    )
    assert [s for s in steps if not s.startswith("#")] == [
        "playwright_version=$(npm view @playwright/mcp version)",
        f"add_user_mcp_server playwright npx {pinned} --headless --browser chromium",
        f"npx -y -p {pinned} playwright install chromium",
        "sudo -v",
        f"{root_npx} playwright install-deps chromium",
    ]


def test_configure_codex_runs_after_registration() -> None:
    command = (
        'uv run --project "$dotfiles_root" --no-dev python '
        '"$dotfiles_root/tools/configure_codex.py" "$HOME/.codex/config.toml"'
    )
    assert command in LINES
    assert find(r"^add_user_mcp_server lgtmcp") < find(r"configure_codex\.py")


def test_codex_instructions_are_generated_into_an_existing_directory() -> None:
    mkdir = find(r'^mkdir -p "\$HOME/\.codex"$')
    assert mkdir + 1 == find(r"^\{$")
    assert '} >"$HOME/.codex/AGENTS.md"' in LINES


def test_linux_gets_git_credential_helpers_and_claude_onboarding() -> None:
    credential = find(r"git config --file .*credential")
    guard = find(r"^if \[\[ \$os == linux \]\]; then", find(r"configure_codex\.py"))
    assert guard < credential
    body = "\n".join(block(guard))
    assert "https://github.com https://gist.github.com" in body
    assert "'!gh auth git-credential'" in body
    assert "$HOME/.config/git/config" in body
    assert "hasCompletedOnboarding = true" in body
    assert ".projects[$root].hasTrustDialogAccepted = true" in body
    assert '--arg root "$dotfiles_root"' in body
    assert 'mv "$claude_json_new" "$claude_json"' in body


def run_claude_json_update(home: Path, root: Path) -> subprocess.CompletedProcess[str]:
    start = find(r"^\s+claude_json=\$HOME/\.claude\.json$")
    end = find(r'^\s+mv "\$claude_json_new" "\$claude_json"$', start)
    script = "set -euo pipefail\n" + "\n".join(LINES[start : end + 1])
    return subprocess.run(
        ["/bin/bash", "-c", script],
        env={
            "PATH": os.environ["PATH"],
            "HOME": str(home),
            "dotfiles_root": str(root),
        },
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )


def test_claude_json_update_keeps_what_the_file_already_holds(tmp_path: Path) -> None:
    claude_json = tmp_path / ".claude.json"
    existing: dict[str, object] = {
        "theme": "dark",
        "projects": {"/other": {"allowedTools": []}},
    }
    _ = claude_json.write_text(json.dumps(existing))
    root = tmp_path / "dotfiles"
    result = run_claude_json_update(tmp_path, root)
    assert result.returncode == 0, result.stderr
    assert json.loads(claude_json.read_text()) == {
        "theme": "dark",
        "hasCompletedOnboarding": True,
        "projects": {
            "/other": {"allowedTools": []},
            str(root): {"hasTrustDialogAccepted": True},
        },
    }
    assert [entry.name for entry in tmp_path.iterdir()] == [".claude.json"]


def test_claude_json_update_creates_a_missing_file_for_its_owner_only(
    tmp_path: Path,
) -> None:
    root = tmp_path / "dotfiles"
    result = run_claude_json_update(tmp_path, root)
    assert result.returncode == 0, result.stderr
    claude_json = tmp_path / ".claude.json"
    assert json.loads(claude_json.read_text()) == {
        "hasCompletedOnboarding": True,
        "projects": {str(root): {"hasTrustDialogAccepted": True}},
    }
    assert stat.S_IMODE(claude_json.stat().st_mode) == 0o600


def test_claude_json_update_refuses_a_file_that_jq_prints_nothing_for(
    tmp_path: Path,
) -> None:
    claude_json = tmp_path / ".claude.json"
    _ = claude_json.write_text("\n")
    result = run_claude_json_update(tmp_path, tmp_path / "dotfiles")
    assert result.returncode == 1
    assert "cannot update" in result.stderr
    assert claude_json.read_text() == "\n"
    assert [entry.name for entry in tmp_path.iterdir()] == [".claude.json"]


def test_known_hosts_then_go_telemetry_then_mcp_registration() -> None:
    assert find(r"Bootstrap TLS trust") < find(r"^go telemetry on")
    assert find(r"^go telemetry on") < find(r"^add_user_mcp_server\(\)")


HOMEBREW_INSTALLER = (
    "curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
)
ZSH_DEFER = "$HOME/.local/share/zsh-defer"
BREW_OUTDATED = "brew outdated --greedy-auto-updates --cask --quiet"
KNOWN_HOSTS = '"$HOME/.ssh/known_hosts"'
QUIET_PIP = " | (grep -v '^Requirement already satisfied:' || true)"

# The macOS run's commands, in order. Lines may be added between them, but none
# may change, move or disappear.
MACOS_SPINE = (
    "cat .codex/instructions.md",
    'UNAME_MACHINE="$(/usr/bin/uname -m)"',
    'if [[ ${UNAME_MACHINE} == "arm64" ]]; then',
    'HOMEBREW_PREFIX="/opt/homebrew"',
    'HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}"',
    'HOMEBREW_PREFIX="/usr/local"',
    'HOMEBREW_REPOSITORY="${HOMEBREW_PREFIX}/Homebrew"',
    "if [[ ! -d $HOMEBREW_REPOSITORY ]]; then",
    f'CI=1 /bin/bash -c "$({HOMEBREW_INSTALLER})"',
    'eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"',
    "brew analytics off",
    "macos_preflight",
    'git clone --depth=1 https://github.com/ohmyzsh/ohmyzsh "$HOME/.oh-my-zsh"',
    '"$HOME/.oh-my-zsh/tools/upgrade.sh" -v minimal',
    f'git clone --depth=1 https://github.com/romkatv/zsh-defer "{ZSH_DEFER}"',
    f'git -C "{ZSH_DEFER}" pull --ff-only',
    '(cd "$HOME/.oh-my-zsh/custom/plugins/fzf-tab" && git pull)',
    '(cd "$HOME/.oh-my-zsh/custom/plugins/git-prompt-watcher" && git pull)',
    "brew update",
    (
        'brew bundle --force --no-upgrade --upgrade-formulae="$brew_formulae" | '
        "(grep -v '^Using ' || true)"
    ),
    "brew bundle cleanup --force",
    f"{BREW_OUTDATED} | sed '/^google-chrome$/d' | xargs -r brew upgrade --cask --yes",
    "brew autoremove",
    (
        'if ! chezmoi --source "$dotfiles_root" '
        "--less-interactive --error-on-conflict apply; then"
    ),
    "brew cleanup --prune=all",
    "uv cache prune",
    "go clean -modcache",
    "bin/docker-prune",
    "macos_xcode",
    "macos_login_shell",
    "export PIP_DISABLE_PIP_VERSION_CHECK=1",
    "datasette install --upgrade datasette-cluster-map" + QUIET_PIP,
    "llm install --upgrade llm-{gemini,anthropic,perplexity,cmd,openai-plugin}"
    + QUIET_PIP,
    "gcloud --quiet components update",
    "macos_defaults",
    "emacs --batch --script .emacs.d/provision.el",
    "macos_emacs_app",
    f"if [ ! -f {KNOWN_HOSTS} ] || ! grep -q '^github\\.com ' {KNOWN_HOSTS}; then",
    "https://api.github.com/meta |",
    "jq -r '.ssh_keys[]' |",
    f"sed -e 's/^/github.com /' >>{KNOWN_HOSTS}",
    "go telemetry on",
    'add_user_mcp_server lgtmcp "$lgtmcp" -tools review_and_commit',
    "add_user_mcp_server playwright npx @playwright/mcp@latest --headless",
    (
        'uv run --project "$dotfiles_root" --no-dev python '
        '"$dotfiles_root/tools/configure_codex.py" "$HOME/.codex/config.toml"'
    ),
    "macos_finish",
)


def test_the_macos_sequence_keeps_its_shared_lines_in_order() -> None:
    stripped = [line.strip() for line in LINES]
    position = 0
    for expected in MACOS_SPINE:
        assert expected in stripped[position:], expected
        position = stripped.index(expected, position) + 1


DHCP_PLIST = (
    "/Library/Preferences/SystemConfiguration/com.apple.InternetSharing.default.plist"
)

# Any `||` or `&&` after these would switch errexit off for the command.
MACOS_SH_PLAIN_COMMANDS = (
    "softwareupdate --install --recommended",
    "softwareupdate --install-rosetta --agree-to-license",
    "sudo xcode-select --reset",
    "sudo xcodebuild -license accept",
    "sudo xcodebuild -runFirstLaunch",
    "xcodebuild -downloadPlatform iOS",
    'sudo dscl . change "$HOME" UserShell /bin/zsh "$HOMEBREW_PREFIX/bin/zsh"',
    "sudo defaults write com.apple.universalaccess mouseDriverCursorSize -float 1.5",
    "sudo defaults write com.apple.universalaccess reduceTransparency true",
    "sudo defaults write com.apple.universalaccess stickyKey false",
    "sudo defaults write com.apple.universalaccess stickyKeyBeepOnModifier false",
    "sudo defaults write com.apple.universalaccess stickyKeysLocation -int 1",
    "defaults import com.googlecode.iterm2 - <.iTerm2/com.googlecode.iterm2.plist",
    "sudo systemsetup -setnetworktimeserver time.google.com",
    "sudo defaults write " + DHCP_PLIST + " bootpd -dict DHCPLeaseTimeSecs -int 600",
    "killall ControlCenter Finder cfprefsd",
    'desktoppr "$HOME/Library/Application Support/desktoppr/navy_blue.png"',
    "rm -rf /Applications/Emacs.app",
    "cupertino setup --keep-existing",
    "launchctl bootstrap gui/$UID",
)


def test_macos_sh_commands_that_must_stop_the_run_are_plain() -> None:
    stripped = {line.strip() for line in MACOS.read_text().splitlines()}
    assert [c for c in MACOS_SH_PLAIN_COMMANDS if c not in stripped] == []


EMUDHRA_HARNESS = r"""
set -euo pipefail
sudo() {
    printf '%s\n' "$*" >>"$SUDO_LOG"
    if [[ $FAIL_COMMAND == sudo || $1 == "$FAIL_COMMAND" ]]; then
        return 7
    fi
    "$@"
}
awk() {
    if [[ $FAIL_COMMAND == awk ]]; then
        printf 'partial output\n'
        return 7
    fi
    command awk "$@"
}
python3() { printf '%s\n' "$STDLIB"; }
stat() {
    if [[ $(uname -s) == Darwin ]]; then
        command stat "$@"
    else
        command stat -c '%a' "$3"
    fi
}
source "$1"
macos_emudhra_workaround
echo finished
"""


def run_emudhra(
    tmp_path: Path, *, fail_command: str = ""
) -> subprocess.CompletedProcess[str]:
    script = tmp_path / "macos.sh"
    _ = script.write_text(
        MACOS.read_text()
        .replace(
            "bundle=/etc/ssl/cert.pem",
            f"bundle={shlex.quote(str(tmp_path / 'cert.pem'))}",
        )
        .replace("/usr/bin/python3", "python3")
    )
    stdlib = tmp_path / "stdlib"
    stdlib.mkdir(exist_ok=True)
    return subprocess.run(
        ["/bin/bash", "-c", EMUDHRA_HARNESS, "bash", str(script)],
        env={
            "PATH": "/usr/bin:/bin",
            "STDLIB": str(stdlib),
            "FAIL_COMMAND": fail_command,
            "SUDO_LOG": str(tmp_path / "sudo.log"),
        },
        capture_output=True,
        text=True,
        check=False,
        timeout=15,
    )


def certificate_section(common_name: str) -> str:
    return (
        f"=== /C=US/O=Example/CN={common_name}\n"
        "Certificate:\n"
        "-----BEGIN CERTIFICATE-----\n"
        f"{common_name}\n"
        "-----END CERTIFICATE-----\n\n"
    )


def emudhra_bundle() -> tuple[str, str]:
    prefix = "### First group\n\n" + certificate_section("Unrelated root")
    middle = certificate_section("AAA Certificate Services Extra")
    suffix = "### Last group\n\n" + certificate_section("emSign Root CA - G1")
    original = (
        prefix
        + certificate_section("AAA Certificate Services")
        + middle
        + certificate_section("emSign ECC Root CA - G3")
        + suffix
        + certificate_section("AAA Certificate Services")
    )
    return original, prefix + middle + suffix


def test_emudhra_removes_only_affected_roots_and_is_idempotent(tmp_path: Path) -> None:
    bundle = tmp_path / "cert.pem"
    original, expected = emudhra_bundle()
    _ = bundle.write_text(original)
    bundle.chmod(0o644)
    original_stat = bundle.stat()
    result = run_emudhra(tmp_path)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "finished\n"
    assert bundle.read_text() == expected
    backup = tmp_path / "cert.pem.bak"
    assert backup.read_text() == original
    for cert_file in (bundle, backup):
        info = cert_file.stat()
        assert stat.S_IMODE(info.st_mode) == stat.S_IMODE(original_stat.st_mode)
        assert (info.st_uid, info.st_gid) == (
            original_stat.st_uid,
            original_stat.st_gid,
        )
    modified = bundle.stat().st_mtime_ns
    result = run_emudhra(tmp_path, fail_command="sudo")
    assert result.returncode == 0, result.stderr
    assert bundle.read_text() == expected
    assert bundle.stat().st_mtime_ns == modified
    assert backup.read_text() == original
    assert sorted(p.name for p in tmp_path.glob("cert.pem.*")) == ["cert.pem.bak"]


def test_emudhra_filters_updated_bundle_without_restoring_old_roots(
    tmp_path: Path,
) -> None:
    bundle = tmp_path / "cert.pem"
    original, expected = emudhra_bundle()
    _ = bundle.write_text(original)
    result = run_emudhra(tmp_path)
    assert result.returncode == 0, result.stderr
    added = certificate_section("New root")
    _ = bundle.write_text(
        expected + added + certificate_section("AAA Certificate Services")
    )
    result = run_emudhra(tmp_path)
    assert result.returncode == 0, result.stderr
    assert bundle.read_text() == expected + added
    assert (tmp_path / "cert.pem.bak").read_text() == original


@pytest.mark.parametrize("existing", [None, "existing_customization = True"])
def test_emudhra_configures_pip_once_preserving_python_customizations(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, existing: str | None
) -> None:
    _ = (tmp_path / "cert.pem").write_text(certificate_section("Unaffected root"))
    stdlib = tmp_path / "stdlib"
    stdlib.mkdir()
    sitecustomize = stdlib / "sitecustomize.py"
    if existing is not None:
        _ = sitecustomize.write_text(existing)
        sitecustomize.chmod(0o600)
    for fail_command in ("", "sudo"):
        result = run_emudhra(tmp_path, fail_command=fail_command)
        assert result.returncode == 0, result.stderr
    text = sitecustomize.read_text()
    assert text.startswith(existing or "")
    assert text.count('os.environ.setdefault("PIP_CERT", "/etc/ssl/cert.pem")') == 1
    assert stat.S_IMODE(sitecustomize.stat().st_mode) == 0o644
    assert not (tmp_path / "cert.pem.bak").exists()
    monkeypatch.setenv("PIP_CERT", "/custom/ca.pem")
    _ = runpy.run_path(str(sitecustomize))
    assert os.environ["PIP_CERT"] == "/custom/ca.pem"
    monkeypatch.delenv("PIP_CERT")
    _ = runpy.run_path(str(sitecustomize))
    assert os.environ["PIP_CERT"] == "/etc/ssl/cert.pem"


def test_emudhra_only_uses_sudo_for_permissions_when_contents_are_correct(
    tmp_path: Path,
) -> None:
    _ = (tmp_path / "cert.pem").write_text(certificate_section("Unaffected root"))
    result = run_emudhra(tmp_path)
    assert result.returncode == 0, result.stderr
    sitecustomize = tmp_path / "stdlib/sitecustomize.py"
    contents = sitecustomize.read_text()
    modified = sitecustomize.stat().st_mtime_ns
    sitecustomize.chmod(0o600)
    log = tmp_path / "sudo.log"
    _ = log.write_text("")
    result = run_emudhra(tmp_path)
    assert result.returncode == 0, result.stderr
    assert log.read_text().splitlines() == [f"chmod 644 {sitecustomize}"]
    assert sitecustomize.read_text() == contents
    assert sitecustomize.stat().st_mtime_ns == modified
    assert stat.S_IMODE(sitecustomize.stat().st_mode) == 0o644


@pytest.mark.parametrize("fail_command", ["awk", "cp", "tee", "mv"])
def test_emudhra_failure_preserves_bundle_and_stops_provisioning(
    tmp_path: Path, fail_command: str
) -> None:
    bundle = tmp_path / "cert.pem"
    original, _ = emudhra_bundle()
    _ = bundle.write_text(original)
    result = run_emudhra(tmp_path, fail_command=fail_command)
    assert result.returncode == 7, result.stderr
    assert "finished" not in result.stdout
    assert bundle.read_text() == original
    assert not (tmp_path / "stdlib/sitecustomize.py").exists()
    assert all(p.name == "cert.pem.bak" for p in tmp_path.glob("cert.pem.*"))


def test_emudhra_refuses_an_empty_filtered_bundle(tmp_path: Path) -> None:
    bundle = tmp_path / "cert.pem"
    original = certificate_section("AAA Certificate Services")
    _ = bundle.write_text(original)
    result = run_emudhra(tmp_path)
    assert result.returncode != 0
    assert "empty CA bundle" in result.stderr
    assert "finished" not in result.stdout
    assert bundle.read_text() == original
    assert not list(tmp_path.glob("cert.pem.*"))


GATEWAY_INSTALL_HARNESS = r"""
set -euo pipefail
source "$1"
state=$2
remaining=$3
modules_selection=dev
modules_has() { [[ $2 == dev ]]; }
bun() { printf 'bun %s\n' "$*" >>"$LOG"; }
sleep() {
    printf 'wait\n' >>"$LOG"
    remaining=$((remaining - 1))
}
launchctl() {
    printf '%s\n' "$1" >>"$LOG"
    case $1 in
        print)
            if [[ $state == unloading && $remaining -le 0 ]]; then
                state=absent
            fi
            [[ $state != absent ]] || return 113
            ;;
        bootout)
            [[ $BOOTOUT_STATUS -eq 0 ]] || return "$BOOTOUT_STATUS"
            state=unloading
            ;;
        bootstrap)
            if [[ $state != absent ]]; then
                echo 'Bootstrap failed: 5: Input/output error' >&2
                return 5
            fi
            state=loaded
            ;;
        *) return 64 ;;
    esac
}
macos_vm_access_install
echo finished
"""


def run_gateway_install(
    tmp_path: Path,
    *,
    state: str = "loaded",
    delay: int = 0,
    bootout_status: int = 0,
) -> tuple[subprocess.CompletedProcess[str], list[str]]:
    home = tmp_path / "home"
    (home / "bin").mkdir(parents=True)
    command = home / "bin/vm-access"
    _ = command.write_text('#!/bin/sh\nprintf "init\\n" >>"$LOG"\n')
    command.chmod(0o755)
    log = tmp_path / "calls"
    result = subprocess.run(
        [
            "/bin/bash",
            "-c",
            GATEWAY_INSTALL_HARNESS,
            "bash",
            str(MACOS),
            state,
            str(delay),
        ],
        env={
            "HOME": str(home),
            "PATH": "/usr/bin:/bin",
            "LOG": str(log),
            "BOOTOUT_STATUS": str(bootout_status),
        },
        capture_output=True,
        text=True,
        check=False,
        timeout=10,
    )
    return result, log.read_text().splitlines()


@pytest.mark.parametrize("delay", [0, 3])
def test_gateway_reload_waits_for_launchd_to_release_the_label(
    tmp_path: Path, delay: int
) -> None:
    result, calls = run_gateway_install(tmp_path, delay=delay)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "finished\n"
    assert calls.count("bootstrap") == 1
    assert calls.index("bootout") < calls.index("bootstrap")
    if delay:
        assert "wait" in calls


def test_gateway_first_install_loads_without_waiting(tmp_path: Path) -> None:
    result, calls = run_gateway_install(tmp_path, state="absent")
    assert result.returncode == 0, result.stderr
    assert calls.count("bootstrap") == 1
    assert "bootout" not in calls
    assert "wait" not in calls


def test_gateway_reload_fails_if_launchd_does_not_release_the_label(
    tmp_path: Path,
) -> None:
    result, calls = run_gateway_install(tmp_path, delay=1000)
    assert result.returncode != 0
    assert "timed out" in result.stderr
    assert "finished" not in result.stdout
    assert "bootstrap" not in calls


def test_gateway_reload_stops_if_bootout_fails(tmp_path: Path) -> None:
    result, calls = run_gateway_install(tmp_path, bootout_status=7)
    assert result.returncode == 7
    assert "finished" not in result.stdout
    assert "bootstrap" not in calls
    assert "wait" not in calls


MANAGED = {
    ".gitconfig",
    ".config/bat/config",
    ".config/git/ignore",
    ".config/yt-dlp/config",
}


@dataclass
class Chezmoi:
    root: Path
    home: Path
    source: Path
    env: dict[str, str]

    def run(
        self,
        *args: str,
        os_name: str = "linux",
        username: str = "shields",
        check: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                "chezmoi",
                "--source",
                str(self.source),
                "--destination",
                str(self.home),
                "--config",
                str(self.root / "chezmoi.toml"),
                "--persistent-state",
                str(self.root / "state.boltdb"),
                "--cache",
                str(self.root / "cache"),
                "--override-data",
                json.dumps({"chezmoi": {"os": os_name, "username": username}}),
                "--no-tty",
                "--no-pager",
                "--use-builtin-diff",
                "--color=false",
                "--less-interactive",
                "--error-on-conflict",
                *args,
            ],
            env=self.env,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=30,
            check=check,
        )

    def git_value(self, key: str) -> str:
        result = subprocess.run(
            ["git", "config", "--file", str(self.home / ".gitconfig"), "--get", key],
            env=self.env,
            capture_output=True,
            text=True,
            timeout=10,
            check=False,
        )
        assert result.returncode in {0, 1}, result.stderr
        return result.stdout.strip()


@pytest.fixture
def chezmoi(tmp_path: Path) -> Chezmoi:
    home = tmp_path / "home"
    home.mkdir()
    source = tmp_path / "source"
    _ = shutil.copytree(REPO / "home", source / "home")
    _ = shutil.copyfile(REPO / ".chezmoiroot", source / ".chezmoiroot")
    _ = (tmp_path / "chezmoi.toml").write_text("")
    shims = tmp_path / "shims"
    shims.mkdir()
    profiles = shims / "profiles"
    _ = profiles.write_text(
        r"""#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" > "$PROFILES_LOG"
printf '%s\n' "$PROFILES_STATUS"
exit "$PROFILES_EXIT"
"""
    )
    profiles.chmod(0o755)
    env = {
        "HOME": str(home),
        "PATH": f"{shims}{os.pathsep}{os.environ['PATH']}",
        "XDG_CONFIG_HOME": str(home / ".config"),
        "XDG_CACHE_HOME": str(tmp_path / "cache"),
        "XDG_DATA_HOME": str(tmp_path / "data"),
        "PROFILES_LOG": str(tmp_path / "profiles.log"),
        "PROFILES_STATUS": "Enrolled via DEP: No\nMDM enrollment: No",
        "PROFILES_EXIT": "0",
        "GIT_CONFIG_NOSYSTEM": "1",
    }
    return Chezmoi(tmp_path, home, source, env)


@pytest.mark.parametrize(
    ("os_name", "username", "enrollment", "personal"),
    [
        ("linux", "shields", "", True),
        ("linux", "agent", "", False),
        ("darwin", "shields", "Enrolled via DEP: No\nMDM enrollment: No", True),
        ("darwin", "shields", "Enrolled via DEP: No\nMDM enrollment: Yes", False),
        ("darwin", "shields", "Enrolled via DEP: Yes\nMDM enrollment: No", False),
        ("darwin", "other", "", False),
    ],
)
def test_git_identity(
    chezmoi: Chezmoi,
    os_name: str,
    username: str,
    enrollment: str,
    *,
    personal: bool,
) -> None:
    chezmoi.env["PROFILES_STATUS"] = enrollment
    _ = chezmoi.run("apply", os_name=os_name, username=username)
    assert chezmoi.git_value("user.name") == "Michael Shields"
    assert chezmoi.git_value("user.email") == ("shields@msrl.com" if personal else "")
    assert chezmoi.git_value("github.user") == ("shields" if personal else "")
    assert chezmoi.git_value("hook.gitleaks.event") == "pre-commit"
    log = chezmoi.root / "profiles.log"
    if os_name == "darwin" and username == "shields":
        assert log.read_text() == "status -type enrollment\n"
    else:
        assert not log.exists()


def test_enrollment_failure_does_not_install_a_personal_identity(
    chezmoi: Chezmoi,
) -> None:
    chezmoi.env["PROFILES_EXIT"] = "1"
    result = chezmoi.run("apply", os_name="darwin", check=False)
    assert result.returncode != 0
    assert "profiles" in result.stderr
    assert not (chezmoi.home / ".gitconfig").exists()


@pytest.mark.parametrize("os_name", ["linux", "darwin"])
def test_apply_owns_only_the_pilot_files_and_is_idempotent(
    chezmoi: Chezmoi, os_name: str
) -> None:
    credentials = chezmoi.home / ".config/git/config"
    credentials.parent.mkdir(parents=True)
    _ = credentials.write_text('[credential "https://github.com"]\n\thelper = test\n')
    assert set(chezmoi.run("managed", "--include=files").stdout.splitlines()) == MANAGED
    preview = chezmoi.run("diff", os_name=os_name)
    assert ".gitconfig" in preview.stdout
    assert not (chezmoi.home / ".gitconfig").exists()
    _ = chezmoi.run("apply", os_name=os_name)
    assert {
        str(p.relative_to(chezmoi.home)) for p in chezmoi.home.rglob("*") if p.is_file()
    } == MANAGED | {".config/git/config"}
    for name in MANAGED - {".gitconfig"}:
        source = chezmoi.source / "home" / name.replace(".config/", "dot_config/", 1)
        target = chezmoi.home / name
        assert target.read_bytes() == source.read_bytes()
        assert not target.is_symlink()
    modified = {name: (chezmoi.home / name).stat().st_mtime_ns for name in MANAGED}
    _ = chezmoi.run("apply", os_name=os_name)
    _ = chezmoi.run("verify", os_name=os_name)
    assert chezmoi.run("diff", os_name=os_name).stdout == ""
    assert modified == {
        name: (chezmoi.home / name).stat().st_mtime_ns for name in MANAGED
    }
    assert (
        credentials.read_text()
        == '[credential "https://github.com"]\n\thelper = test\n'
    )


def test_source_edits_apply_and_destination_edits_are_protected(
    chezmoi: Chezmoi,
) -> None:
    _ = chezmoi.run("apply")
    source = chezmoi.source / "home/dot_config/bat/config"
    updated = source.read_text().replace("--style=plain", "--style=numbers")
    _ = source.write_text(updated)
    assert "--style=numbers" in chezmoi.run("diff").stdout
    _ = chezmoi.run("apply")
    target = chezmoi.home / ".config/bat/config"
    assert target.read_text() == updated
    local = updated + "\n--paging=never\n"
    _ = target.write_text(local)
    assert "--paging=never" in chezmoi.run("diff").stdout
    assert chezmoi.run("apply", check=False).returncode != 0
    assert target.read_text() == local
    _ = chezmoi.run("re-add", str(target))
    assert source.read_text() == local
    _ = chezmoi.run("apply")
    _ = chezmoi.run("verify")


def test_first_apply_preserves_a_conflicting_existing_file(chezmoi: Chezmoi) -> None:
    target = chezmoi.home / ".gitconfig"
    local = "[user]\n\temail = work@example.com\n"
    _ = target.write_text(local)
    assert chezmoi.run("apply", check=False).returncode != 0
    assert target.read_text() == local


@pytest.mark.parametrize("failure", ["", "conflict", "template"])
def test_provision_chezmoi_apply_reports_failures(
    chezmoi: Chezmoi, failure: str
) -> None:
    target = chezmoi.home / ".gitconfig"
    local = "[user]\n\temail = work@example.com\n"
    if failure == "conflict":
        _ = target.write_text(local)
    elif failure == "template":
        template = chezmoi.source / "home/dot_gitconfig.tmpl"
        _ = template.write_text(template.read_text() + '{{ fail "template failure" }}')
    start = find(r"^if ! chezmoi ")
    snippet = "\n".join([LINES[start], *block(start), "fi"])
    result = subprocess.run(
        ["/bin/bash", "-c", f"set -euo pipefail\n{snippet}\necho finished"],
        env=chezmoi.env | {"dotfiles_root": str(chezmoi.source)},
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )
    if failure:
        assert result.returncode == 1
        assert "chezmoi apply failed" in result.stderr
        assert "make diff" in result.stderr
        assert "README.md" in result.stderr
        assert "finished" not in result.stdout
        if failure == "conflict":
            assert target.read_text() == local
    else:
        assert result.returncode == 0, result.stderr
        assert "finished" in result.stdout
        assert target.exists()
