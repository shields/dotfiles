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

import contextlib
import hashlib
import os
import re
import shlex
import shutil
import signal
import stat
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

import pytest

if TYPE_CHECKING:
    from collections.abc import Mapping

REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "tools/stage_tree.sh"
TAR_VARIANTS = ["tar", *(name for name in ("gtar", "bsdtar") if shutil.which(name))]

GLOBAL_CONFIG = """\
[user]
    name = Test User
    email = test@example.com
[init]
    defaultBranch = main
    templateDir = {template}
[core]
    excludesFile = {ignore}
[protocol "file"]
    allow = always
[maintenance]
    auto = false
[gc]
    auto = 0
"""

REPO_FILES = {
    ".gitignore": "ignored.txt\n*.tmp\nbuild/\n",
    "README.md": "readme\n",
    "src/app.py": "print('app')\n",
    "bin/run": "#!/bin/sh\necho run\n",
}


@dataclass(frozen=True)
class Seed:
    config: Path
    repo: Path


@dataclass(frozen=True)
class Layout:
    base: Path
    repo: Path
    home: Path
    dest: Path
    tmp: Path
    victim: Path
    env: dict[str, str]

    @property
    def script(self) -> Path:
        return self.repo / "tools/stage_tree.sh"

    def git(self, *args: str, cwd: Path | None = None) -> str:
        return run_git(self.env, cwd or self.repo, *args)

    def stage(
        self,
        dest: str | Path | None = None,
        *,
        cwd: Path | None = None,
        script: Path | None = None,
        env: Mapping[str, str | None] | None = None,
        args: list[str] | None = None,
    ) -> subprocess.CompletedProcess[str]:
        environment = dict(self.env)
        for name, value in (env or {}).items():
            if value is None:
                _ = environment.pop(name, None)
            else:
                environment[name] = value
        target = [str(self.dest if dest is None else dest)] if args is None else args
        return subprocess.run(
            [str(script or self.script), *target],
            cwd=cwd or self.base,
            env=environment,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=120,
            check=False,
        )

    def write(self, relative: str, text: str, mode: int | None = None) -> None:
        path = self.repo / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(text)
        if mode is not None:
            path.chmod(mode)

    def commit_all(self, message: str = "commit") -> None:
        _ = self.git("add", "-A")
        _ = self.git("commit", "-q", "-m", message)

    def track_a_file_in_a_staged_context(self) -> None:
        self.write("cloudflare/.context/.git/stage_tree", "")
        self.write("cloudflare/.context/precious.txt", "tracked\n")
        self.commit_all("track it")

    def state(self) -> dict[str, tuple[str, bytes]]:
        found: dict[str, tuple[str, bytes]] = {}
        for path in sorted(self.base.rglob("*")):
            if path.is_relative_to(self.tmp):
                continue
            relative = path.relative_to(self.base).as_posix()
            if path.is_symlink():
                found[relative] = ("link", str(path.readlink()).encode())
            elif path.is_dir():
                found[relative] = ("dir", b"")
            else:
                found[relative] = ("file", path.read_bytes())
        return found


def git_env(config: Path, home: Path) -> dict[str, str]:
    return {
        "PATH": os.environ["PATH"],
        "HOME": str(home),
        "GIT_CONFIG_GLOBAL": str(config),
        "GIT_CONFIG_NOSYSTEM": "1",
    }


def run_git(env: Mapping[str, str], cwd: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=cwd,
        env=env,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    return result.stdout


@pytest.fixture(scope="module")
def seed(tmp_path_factory: pytest.TempPathFactory) -> Seed:
    base = tmp_path_factory.mktemp("seed").resolve()
    template = base / "template"
    (template / "hooks").mkdir(parents=True)
    hook = template / "hooks/pre-commit"
    _ = hook.write_text("#!/bin/sh\nexit 0\n")
    hook.chmod(0o755)
    _ = (base / "global_ignore").write_text("*.log\n")
    config = base / "gitconfig"
    _ = config.write_text(
        GLOBAL_CONFIG.format(template=template, ignore=base / "global_ignore")
    )
    repo = base / "repo"
    repo.mkdir()
    env = git_env(config, base)
    _ = run_git(env, repo, "init", "-q")
    for name, text in REPO_FILES.items():
        path = repo / name
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(text)
    # tar -p carries these modes into the staged tree, where the tests assert
    # them, so neither the umask nor the checkout may decide them.
    (repo / "bin/run").chmod(0o755)
    (repo / "README.md").chmod(0o644)
    (repo / "tools").mkdir()
    _ = shutil.copy2(SCRIPT, repo / "tools/stage_tree.sh")
    (repo / "tools/stage_tree.sh").chmod(0o755)
    (repo / "link-to-readme").symlink_to("README.md")
    _ = run_git(env, repo, "add", "-A")
    _ = run_git(env, repo, "commit", "-q", "-m", "initial")
    return Seed(config=config, repo=repo)


@pytest.fixture
def layout(tmp_path: Path, seed: Seed) -> Layout:
    base = tmp_path.resolve()
    home = base / "users/me"
    repo = base / "work/repo"
    tmp = base / "tmp"
    victim = base / "victim"
    for directory in (home, tmp, victim, repo.parent):
        directory.mkdir(parents=True)
    _ = shutil.copytree(seed.repo, repo, symlinks=True)
    _ = (home / "sentinel").write_text("home\n")
    _ = (victim / "sentinel").write_text("victim\n")
    env = git_env(seed.config, home)
    env["TMPDIR"] = str(tmp)
    return Layout(
        base=base,
        repo=repo,
        home=home,
        dest=base / "stage",
        tmp=tmp,
        victim=victim,
        env=env,
    )


def tree_files(root: Path) -> list[str]:
    return sorted(
        path.relative_to(root).as_posix()
        for path in root.rglob("*")
        if (path.is_file() or path.is_symlink())
        and path.relative_to(root).parts[0] != ".git"
    )


def index_files(layout: Layout, dest: Path) -> list[str]:
    names = layout.git("ls-files", "-z", cwd=dest).split("\0")
    return sorted(name for name in names if name)


def tracked_and_untracked(layout: Layout, cwd: Path | None = None) -> list[str]:
    names = layout.git(
        "ls-files", "-z", "--cached", "--others", "--exclude-standard", cwd=cwd
    ).split("\0")
    root = cwd or layout.repo
    return sorted({name for name in names if name and os.path.lexists(root / name)})


def planted_secret() -> str:
    return "ghp_" + hashlib.sha256(b"stage_tree secret").hexdigest()[:36]


def set_xattr(path: Path, name: str) -> None:
    if sys.platform == "darwin":
        _ = subprocess.run(
            ["xattr", "-w", name, "value", str(path)],
            capture_output=True,
            check=True,
        )


def xattr_names(path: Path) -> set[str]:
    if sys.platform != "darwin":
        return set()
    result = subprocess.run(
        ["xattr", str(path)], capture_output=True, text=True, check=True
    )
    return set(result.stdout.split())


def tool_dir(
    layout: Layout, name: str, links: Mapping[str, str], scripts: Mapping[str, str]
) -> Path:
    directory = layout.base / name
    directory.mkdir()
    for tool, real in links.items():
        found = shutil.which(real)
        assert found, f"{real} is required"
        (directory / tool).symlink_to(found)
    for tool, text in scripts.items():
        path = directory / tool
        _ = path.write_text(text)
        path.chmod(0o755)
    return directory


def path_with(layout: Layout, directory: Path) -> dict[str, str | None]:
    return {"PATH": f"{directory}{os.pathsep}{layout.env['PATH']}"}


def test_stages_the_working_tree_not_the_commit(layout: Layout) -> None:
    layout.write("README.md", "edited but not committed\n")
    layout.write("new/untracked.txt", "untracked\n")
    layout.write("ignored.txt", "ignored by the repo\n")
    layout.write("build/out.o", "ignored directory\n")
    layout.write("debug.log", "ignored by the global excludes\n")
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert (layout.dest / "README.md").read_text() == "edited but not committed\n"
    assert (layout.dest / "new/untracked.txt").read_text() == "untracked\n"
    assert (layout.dest / "src/app.py").read_text() == "print('app')\n"
    for excluded in ("ignored.txt", "build", "debug.log"):
        assert not (layout.dest / excluded).exists()
    expected = tracked_and_untracked(layout)
    assert tree_files(layout.dest) == expected
    assert result.stdout.strip() == (
        f"stage_tree: staged {len(expected)} files into {layout.dest}"
    )
    assert (layout.dest / ".git").is_dir()


def test_deleted_tracked_file_is_left_out_without_error(layout: Layout) -> None:
    (layout.repo / "src/app.py").unlink()
    (layout.repo / "link-to-readme").unlink()
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert not (layout.dest / "src").exists()
    assert not (layout.dest / "link-to-readme").is_symlink()
    assert "src/app.py" not in index_files(layout, layout.dest)
    assert "README.md" in index_files(layout, layout.dest)


@pytest.mark.parametrize("variant", TAR_VARIANTS)
def test_modes_and_symlinks_survive(layout: Layout, variant: str) -> None:
    layout.write("private.txt", "0600\n", 0o600)
    layout.write("tools/helper.sh", "#!/bin/sh\n", 0o755)
    (layout.repo / "dangling").symlink_to("does/not/exist")
    (layout.repo / "link-to-dir").symlink_to("src")
    path = tool_dir(layout, "tar-bin", {"tar": variant}, {})
    result = layout.stage(env=path_with(layout, path))
    assert result.returncode == 0, result.stderr
    assert stat.S_IMODE((layout.dest / "bin/run").stat().st_mode) == 0o755
    assert stat.S_IMODE((layout.dest / "tools/helper.sh").stat().st_mode) == 0o755
    assert stat.S_IMODE((layout.dest / "tools/stage_tree.sh").stat().st_mode) == 0o755
    assert stat.S_IMODE((layout.dest / "private.txt").stat().st_mode) == 0o600
    assert stat.S_IMODE((layout.dest / "README.md").stat().st_mode) == 0o644
    assert (layout.dest / "link-to-readme").readlink() == Path("README.md")
    assert (layout.dest / "dangling").readlink() == Path("does/not/exist")
    assert (layout.dest / "link-to-dir").readlink() == Path("src")
    assert tree_files(layout.dest) == tracked_and_untracked(layout)


def test_no_appledouble_files_or_xattrs_leak_in(layout: Layout) -> None:
    attribute = "com.example.stage_tree"
    set_xattr(layout.repo / "README.md", attribute)
    set_xattr(layout.repo / "bin/run", attribute)
    assert (attribute in xattr_names(layout.repo / "README.md")) == (
        sys.platform == "darwin"
    )
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert list(layout.dest.rglob("._*")) == []
    assert attribute not in xattr_names(layout.dest / "README.md")
    assert attribute not in xattr_names(layout.dest / "bin/run")


def test_rerun_wipes_what_an_earlier_run_left(layout: Layout) -> None:
    first = layout.stage()
    assert first.returncode == 0, first.stderr
    _ = (layout.dest / "stale.txt").write_text("stale\n")
    (layout.dest / "src/stale-dir").mkdir()
    _ = (layout.dest / "src/stale-dir/x").write_text("stale\n")
    (layout.repo / "src/app.py").unlink()
    second = layout.stage()
    assert second.returncode == 0, second.stderr
    assert not (layout.dest / "stale.txt").exists()
    assert not (layout.dest / "src").exists()
    assert tree_files(layout.dest) == tracked_and_untracked(layout)
    assert "stale.txt" not in index_files(layout, layout.dest)


def test_index_matches_the_copy_and_nothing_is_committed(layout: Layout) -> None:
    layout.write("new.txt", "untracked\n")
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert index_files(layout, layout.dest) == tree_files(layout.dest)
    assert index_files(layout, layout.dest) == tracked_and_untracked(layout)
    status = layout.git("status", "--porcelain", cwd=layout.dest).splitlines()
    assert status
    assert all(line.startswith("A  ") for line in status)
    head = subprocess.run(
        ["git", "-C", str(layout.dest), "rev-parse", "--verify", "-q", "HEAD"],
        env=layout.env,
        capture_output=True,
        check=False,
    )
    assert head.returncode != 0
    assert layout.git("rev-parse", "--git-dir", cwd=layout.dest).strip() == ".git"


def test_the_new_repository_is_configured_for_a_case_sensitive_consumer(
    layout: Layout,
) -> None:
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    ignorecase = layout.git("config", "--local", "core.ignorecase", cwd=layout.dest)
    assert ignorecase.strip() == "false"


def test_the_new_repository_ignores_the_callers_templates(layout: Layout) -> None:
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert (layout.repo / ".git/hooks/pre-commit").exists()
    assert not (layout.dest / ".git/hooks").exists()


def test_global_and_repo_ignores_cannot_hide_a_tracked_file(layout: Layout) -> None:
    layout.write("kept.log", "tracked although the global excludes match\n")
    layout.write("kept.tmp", "tracked although .gitignore matches\n")
    _ = layout.git("add", "-f", "kept.log", "kept.tmp")
    layout.write("untracked.log", "globally ignored and untracked\n")
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    names = index_files(layout, layout.dest)
    assert "kept.log" in names
    assert "kept.tmp" in names
    assert "untracked.log" not in names
    assert not (layout.dest / "untracked.log").exists()


@pytest.mark.parametrize("variant", TAR_VARIANTS)
def test_awkward_names_survive(layout: Layout, variant: str) -> None:
    names = [
        "with space.txt",
        "-dash.txt",
        "-C sub",
        "-Cdir.txt",
        "@at.txt",
        "multi\nline.txt",
        "a:b",
        "c:/d.txt",
    ]
    for name in names:
        layout.write(name, f"{name!r}\n")
    path = tool_dir(layout, "tar-bin", {"tar": variant}, {})
    result = layout.stage(env=path_with(layout, path))
    assert result.returncode == 0, result.stderr
    for name in names:
        assert (layout.dest / name).read_text() == f"{name!r}\n"
    assert index_files(layout, layout.dest) == tracked_and_untracked(layout)


@pytest.mark.parametrize("variant", TAR_VARIANTS)
def test_a_non_ascii_name_is_kept_or_the_run_fails(
    layout: Layout, variant: str
) -> None:
    name = "\u00fcn\u00ef.txt"
    layout.write(name, "non-ascii\n")
    path = tool_dir(layout, "tar-bin", {"tar": variant}, {})
    result = layout.stage(env=path_with(layout, path))
    version = subprocess.run(
        [str(path / "tar"), "--version"],
        capture_output=True,
        text=True,
        timeout=15,
        check=True,
    ).stdout
    if sys.platform == "darwin" and "GNU tar" not in version:
        assert result.returncode != 0
        assert "differ from the source file names" in result.stderr
        assert not layout.dest.exists()
    else:
        assert result.returncode == 0, result.stderr
        assert name in [entry.name for entry in layout.dest.iterdir()]
        assert index_files(layout, layout.dest) == tracked_and_untracked(layout)


def test_an_unresolved_merge_is_staged_as_it_stands(layout: Layout) -> None:
    _ = layout.git("switch", "-q", "-c", "other")
    layout.write("README.md", "other side\n")
    layout.commit_all("other")
    _ = layout.git("switch", "-q", "main")
    layout.write("README.md", "main side\n")
    layout.commit_all("main")
    merge = subprocess.run(
        ["git", "merge", "other"],
        cwd=layout.repo,
        env=layout.env,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    assert merge.returncode != 0
    assert "<<<<<<<" in (layout.repo / "README.md").read_text()
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert "<<<<<<<" in (layout.dest / "README.md").read_text()
    expected = tracked_and_untracked(layout)
    assert tree_files(layout.dest) == expected
    assert index_files(layout, layout.dest) == expected
    assert f"staged {len(expected)} files" in result.stdout


def test_cdpath_does_not_redirect_a_relative_destination(layout: Layout) -> None:
    (layout.base / "sub").mkdir()
    decoy = layout.base / "decoy"
    (decoy / "sub").mkdir(parents=True)
    result = layout.stage("sub/stage", cwd=layout.base, env={"CDPATH": str(decoy)})
    assert result.returncode == 0, result.stderr
    assert (layout.base / "sub/stage/README.md").exists()
    assert list((decoy / "sub").iterdir()) == []


@pytest.mark.parametrize("variant", TAR_VARIANTS)
def test_modes_do_not_depend_on_the_umask(layout: Layout, variant: str) -> None:
    wrapper = f'#!/bin/sh\numask 077\nexec {layout.script} "$@"\n'
    path = tool_dir(layout, "umask-bin", {"tar": variant}, {"run": wrapper})
    result = layout.stage(script=path / "run", env=path_with(layout, path))
    assert result.returncode == 0, result.stderr
    assert stat.S_IMODE((layout.dest / "bin/run").stat().st_mode) == 0o755
    assert stat.S_IMODE((layout.dest / "README.md").stat().st_mode) == 0o644


def test_submodules_and_nested_repositories_are_not_copied(layout: Layout) -> None:
    upstream = layout.base / "upstream"
    upstream.mkdir()
    _ = layout.git("init", "-q", cwd=upstream)
    _ = (upstream / "lib.txt").write_text("lib\n")
    _ = layout.git("add", "-A", cwd=upstream)
    _ = layout.git("commit", "-q", "-m", "lib", cwd=upstream)
    _ = layout.git("submodule", "add", "-q", str(upstream), "vendor/lib")
    layout.commit_all("add a submodule")
    nested = layout.repo / "scratch"
    nested.mkdir()
    _ = layout.git("init", "-q", cwd=nested)
    _ = (nested / "notes.txt").write_text("nested\n")
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert not (layout.dest / "vendor/lib").exists()
    assert not (layout.dest / "scratch").exists()
    assert (layout.dest / ".gitmodules").exists()
    assert list(layout.dest.rglob(".git")) == [layout.dest / ".git"]
    assert "skipping vendor/lib " in result.stderr
    assert "skipping scratch/ " in result.stderr
    assert tree_files(layout.dest) == [
        name
        for name in tracked_and_untracked(layout)
        if name not in {"vendor/lib", "scratch/"}
    ]


def test_a_planted_secret_fails_the_run_and_names_the_file(layout: Layout) -> None:
    secret = planted_secret()
    layout.write("config/prod.env", f"GITHUB_TOKEN={secret}\n")
    result = layout.stage()
    assert result.returncode != 0
    assert "config/prod.env" in result.stderr
    assert "github-pat" in result.stderr
    assert secret not in result.stderr + result.stdout
    assert "staged" not in result.stdout
    assert not layout.dest.exists()
    assert list(layout.tmp.iterdir()) == []


def test_a_secret_in_an_ignored_file_is_not_staged(layout: Layout) -> None:
    layout.write("ignored.txt", f"GITHUB_TOKEN={planted_secret()}\n")
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert not (layout.dest / "ignored.txt").exists()


@pytest.mark.parametrize(
    ("links", "scripts", "search", "message"),
    [
        ({"git": "git", "tar": "tar"}, {}, "alone", "gitleaks is not installed"),
        ({}, {"gitleaks": "#!/bin/sh\nexit 1\n"}, "first", "no dir command"),
        ({}, {"tar": "#!/bin/sh\necho 'BusyBox tar'\n"}, "first", "unsupported tar"),
    ],
    ids=["no-gitleaks", "old-gitleaks", "unsupported-tar"],
)
def test_unusable_tools_fail_before_anything_is_deleted(
    layout: Layout,
    links: dict[str, str],
    scripts: dict[str, str],
    search: str,
    message: str,
) -> None:
    path = tool_dir(layout, "tools", links, scripts)
    layout.dest.mkdir()
    _ = (layout.dest / "keep.txt").write_text("keep\n")
    before = layout.state()
    result = layout.stage(
        env={"PATH": str(path)} if search == "alone" else path_with(layout, path)
    )
    assert result.returncode != 0
    assert message in result.stderr
    assert layout.state() == before


def test_a_failing_tar_leaves_no_destination(layout: Layout) -> None:
    script = '#!/bin/sh\ncase $1 in --version) echo "bsdtar 3.0" ;; *) exit 3 ;; esac\n'
    path = tool_dir(layout, "bad-tar", {}, {"tar": script})
    result = layout.stage(env=path_with(layout, path))
    assert result.returncode != 0
    assert not layout.dest.exists()
    assert list(layout.tmp.iterdir()) == []


def test_an_index_that_misses_a_copied_file_fails_the_run(layout: Layout) -> None:
    real_git = shutil.which("git")
    stub = (
        "#!/bin/sh\n"
        'for arg; do [ "$arg" = add ] && exit 0; done\n'
        f'exec {real_git} "$@"\n'
    )
    path = tool_dir(layout, "lazy-git", {}, {"git": stub})
    result = layout.stage(env=path_with(layout, path))
    assert result.returncode != 0
    assert "the index holds 0 files but" in result.stderr
    assert not layout.dest.exists()


def test_a_tree_with_nothing_to_copy_is_refused(layout: Layout) -> None:
    empty = layout.base / "empty"
    (empty / "tools").mkdir(parents=True)
    _ = shutil.copy2(SCRIPT, empty / "tools/stage_tree.sh")
    _ = layout.git("init", "-q", cwd=empty)
    (empty / ".git/info").mkdir(exist_ok=True)
    _ = (empty / ".git/info/exclude").write_text("tools/\n")
    result = layout.stage(script=empty / "tools/stage_tree.sh")
    assert result.returncode != 0
    assert "no files to stage" in result.stderr
    assert not layout.dest.exists()


def test_a_terminated_run_leaves_no_destination(layout: Layout) -> None:
    # The group is signalled once, as a terminal does, after tar has started. A
    # signal that arrives while the script is removing its files cuts the
    # removal short.
    started = layout.base / "creating"
    stub = (
        "#!/bin/sh\n"
        "case $1 in --version) echo 'bsdtar 3.0'; exit 0 ;; esac\n"
        f": >{shlex.quote(str(started))}\n"
        "exec sleep 60\n"
    )
    path = tool_dir(layout, "waiting-tar", {}, {"tar": stub})
    process = subprocess.Popen(
        [str(layout.script), str(layout.dest)],
        cwd=layout.base,
        env={**layout.env, "PATH": f"{path}{os.pathsep}{layout.env['PATH']}"},
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    try:
        deadline = time.monotonic() + 60
        while not started.exists():
            assert time.monotonic() < deadline, "the script never ran tar"
            time.sleep(0.05)
        os.killpg(process.pid, signal.SIGTERM)
        assert process.wait(timeout=60) == -signal.SIGTERM
    finally:
        with contextlib.suppress(ProcessLookupError, PermissionError):
            os.killpg(process.pid, signal.SIGKILL)
        _ = process.wait()
    assert not layout.dest.exists()
    assert list(layout.tmp.iterdir()) == []


def test_the_archive_passes_through_a_file_not_a_pipe() -> None:
    lines = SCRIPT.read_text().splitlines()
    tars = [line for line in lines if re.search(r"\btar -[cx]\b", line)]
    assert len(tars) == 2
    assert all('"$work/tree.tar"' in line and not line.endswith("|") for line in tars)


def test_git_environment_from_a_hook_does_not_reach_the_source(
    layout: Layout,
) -> None:
    index = layout.repo / ".git/index"
    before = index.read_bytes()
    result = layout.stage(
        env={
            "GIT_DIR": str(layout.repo / ".git"),
            "GIT_WORK_TREE": str(layout.repo),
            "GIT_INDEX_FILE": str(layout.base / "elsewhere-index"),
        }
    )
    assert result.returncode == 0, result.stderr
    assert index.read_bytes() == before
    assert not (layout.base / "elsewhere-index").exists()
    assert index_files(layout, layout.dest) == tracked_and_untracked(layout)


def test_a_linked_worktree_stages_its_own_files(layout: Layout) -> None:
    worktree = layout.base / "work/wt"
    _ = layout.git("worktree", "add", "-q", "-b", "side", str(worktree))
    _ = (worktree / "only-here.txt").write_text("worktree\n")
    result = layout.stage(script=worktree / "tools/stage_tree.sh")
    assert result.returncode == 0, result.stderr
    assert (layout.dest / "only-here.txt").exists()
    assert (layout.dest / ".git").is_dir()
    assert tree_files(layout.dest) == tracked_and_untracked(layout, worktree)


@pytest.mark.parametrize(
    "dest",
    [
        "/",
        "//",
        "{home}",
        "{home}/",
        "{home_alias}",
        "{users}",
        "{repo}",
        "{repo}/",
        "{repo}/tools/..",
        "{repo}/.",
        "{work}",
        "{base}",
        "{repo}/.git",
        "{repo}/.git/stage",
        "{repo}/.git/.context",
        "{repo}/out",
        "{repo}/tools",
        "{repo}/src/.context/..",
        "{base}/missing/stage",
        "{link}",
        "{link}/",
        "{link_to_repo}",
        "{base}/..",
        "{base}/users-alias/me",
        "{base}/work-alias/repo",
    ],
)
def test_unsafe_destinations_are_refused_and_nothing_changes(
    layout: Layout, dest: str
) -> None:
    (layout.base / "users/me-alias").symlink_to("me")
    (layout.base / "link").symlink_to("victim")
    (layout.base / "link-to-repo").symlink_to("work/repo")
    (layout.base / "users-alias").symlink_to("users")
    (layout.base / "work-alias").symlink_to("work")
    _ = (layout.base / "work/sentinel").write_text("work\n")
    target = dest.format(
        home=layout.home,
        home_alias=layout.base / "users/me-alias",
        users=layout.base / "users",
        repo=layout.repo,
        work=layout.base / "work",
        base=layout.base,
        link=layout.base / "link",
        link_to_repo=layout.base / "link-to-repo",
    )
    environment = {"HOME": str(layout.base / "users/me-alias")}
    before = layout.state()
    result = layout.stage(target, env=environment)
    assert result.returncode != 0
    assert result.stderr.startswith("stage_tree: ")
    assert result.stdout == ""
    assert layout.state() == before
    assert list(layout.tmp.iterdir()) == []


def test_an_empty_destination_is_refused(layout: Layout) -> None:
    before = layout.state()
    result = layout.stage("")
    assert result.returncode != 0
    assert "DEST is empty" in result.stderr
    assert layout.state() == before


@pytest.mark.parametrize("args", [[], ["one", "two"]])
def test_wrong_argument_counts_print_usage(layout: Layout, args: list[str]) -> None:
    result = layout.stage(args=args)
    assert result.returncode != 0
    assert "usage: tools/stage_tree.sh DEST" in result.stderr


def test_a_missing_home_is_refused(layout: Layout) -> None:
    result = layout.stage(env={"HOME": None})
    assert result.returncode != 0
    assert "HOME is not set" in result.stderr
    assert not layout.dest.exists()


def test_a_symlinked_destination_leaves_its_target_alone(layout: Layout) -> None:
    link = layout.base / "link"
    link.symlink_to("victim")
    result = layout.stage(link)
    assert result.returncode != 0
    assert "is a symlink" in result.stderr
    assert (layout.victim / "sentinel").read_text() == "victim\n"
    assert link.readlink() == Path("victim")


def test_a_destination_may_sit_in_the_home_directory(layout: Layout) -> None:
    result = layout.stage(layout.home / "stage")
    assert result.returncode == 0, result.stderr
    assert (layout.home / "stage/README.md").exists()
    assert (layout.home / "sentinel").exists()


def test_a_missing_home_directory_does_not_block_staging(layout: Layout) -> None:
    result = layout.stage(env={"HOME": str(layout.base / "no-such-home")})
    assert result.returncode == 0, result.stderr


def test_relative_and_trailing_slash_destinations(layout: Layout) -> None:
    result = layout.stage("stage/", cwd=layout.base)
    assert result.returncode == 0, result.stderr
    assert (layout.base / "stage/README.md").exists()
    _ = (layout.base / "stage/stale").write_text("stale\n")
    result = layout.stage("../stage", cwd=layout.victim)
    assert result.returncode == 0, result.stderr
    assert not (layout.base / "stage/stale").exists()
    assert (layout.base / "stage/README.md").exists()


def test_a_destination_below_a_symlinked_directory_lands_in_the_target(
    layout: Layout,
) -> None:
    (layout.base / "alias").symlink_to("victim")
    result = layout.stage(layout.base / "alias/stage")
    assert result.returncode == 0, result.stderr
    assert (layout.victim / "stage/README.md").exists()
    assert (layout.victim / "sentinel").exists()


def test_a_context_directory_in_the_repository_is_allowed(layout: Layout) -> None:
    (layout.repo / "cloudflare").mkdir()
    context = layout.repo / "cloudflare/.context"
    first = layout.stage(context)
    assert first.returncode == 0, first.stderr
    _ = (context / "stale.txt").write_text("stale\n")
    second = layout.stage("cloudflare/.context", cwd=layout.repo)
    assert second.returncode == 0, second.stderr
    assert first.stderr == ""
    assert second.stderr == ""
    assert not (context / "stale.txt").exists()
    assert not (context / "cloudflare").exists()
    assert (context / "README.md").exists()
    assert (context / ".git").is_dir()
    assert second.stdout.split()[2] == first.stdout.split()[2]


def test_a_context_directory_with_tracked_files_is_refused(layout: Layout) -> None:
    layout.track_a_file_in_a_staged_context()
    before = layout.state()
    result = layout.stage(layout.repo / "cloudflare/.context")
    assert result.returncode != 0
    assert "tracked files" in result.stderr
    assert layout.state() == before


def test_a_linked_worktrees_main_checkout_and_git_directory_are_refused(
    layout: Layout,
) -> None:
    worktree = layout.base / "work/wt"
    _ = layout.git("worktree", "add", "-q", "-b", "side", str(worktree))
    script = worktree / "tools/stage_tree.sh"
    git_dir = layout.repo / ".git/worktrees/wt"
    for dest in (
        layout.repo,
        layout.repo / ".git",
        git_dir,
        git_dir / "stage",
        worktree,
    ):
        before = layout.state()
        result = layout.stage(dest, script=script)
        assert result.returncode != 0, dest
        assert result.stderr.startswith("stage_tree: "), dest
        assert layout.state() == before, dest


def test_a_script_below_the_repository_top_level_is_refused(layout: Layout) -> None:
    nested = layout.repo / "sub/tools"
    nested.mkdir(parents=True)
    script = nested / "stage_tree.sh"
    _ = shutil.copy2(SCRIPT, script)
    result = layout.stage(script=script)
    assert result.returncode != 0
    assert "is not the top level" in result.stderr
    assert not layout.dest.exists()


@pytest.mark.parametrize(
    ("location", "dest"),
    [
        ("{base}/wts/wt", "{base}/wts"),
        ("{repo}/.claude/worktrees/wt", "{repo}/.claude/worktrees"),
        ("{repo}/.claude/worktrees/wt", "{repo}/.claude"),
    ],
)
def test_a_linked_worktrees_ancestors_without_the_git_directory_are_refused(
    layout: Layout, location: str, dest: str
) -> None:
    worktree = Path(location.format(base=layout.base, repo=layout.repo))
    worktree.parent.mkdir(parents=True)
    _ = layout.git("worktree", "add", "-q", "-b", "side", str(worktree))
    before = layout.state()
    result = layout.stage(
        dest.format(base=layout.base, repo=layout.repo),
        script=worktree / "tools/stage_tree.sh",
    )
    assert result.returncode != 0
    assert result.stderr.startswith("stage_tree: ")
    assert f"it is or contains {worktree}" in result.stderr
    assert layout.state() == before


def is_case_insensitive(layout: Layout) -> bool:
    return (layout.base / "USERS").exists()


@pytest.mark.parametrize(
    ("dest", "refused_on_any_volume"),
    [
        ("{users}/ME", False),
        ("{work}/REPO", False),
        ("{base}/WORK", False),
        ("{repo}/.GIT/.context", True),
        ("{repo}/CLOUDFLARE/.context", True),
    ],
)
def test_other_spellings_of_a_protected_destination_are_refused(
    layout: Layout, dest: str, *, refused_on_any_volume: bool
) -> None:
    layout.track_a_file_in_a_staged_context()
    before = layout.state()
    result = layout.stage(
        dest.format(
            users=layout.base / "users",
            work=layout.base / "work",
            base=layout.base,
            repo=layout.repo,
        )
    )
    if refused_on_any_volume or is_case_insensitive(layout):
        assert result.returncode != 0
        assert result.stderr.startswith("stage_tree: ")
        assert layout.state() == before
    else:
        assert result.returncode == 0, result.stderr
    after = layout.state()
    assert all(after.get(name) == entry for name, entry in before.items())


def test_a_relative_missing_home_does_not_block_staging(layout: Layout) -> None:
    result = layout.stage(env={"HOME": "no-such-home"})
    assert result.returncode == 0, result.stderr


@pytest.mark.parametrize(
    ("dest", "cwd"),
    [
        ("{victim}", "{base}"),
        ("{victim}", "{victim}"),
        ("{victim}", "{victim}/child"),
        ("{victim}/child", "{base}"),
    ],
    ids=["elsewhere", "current-directory", "parent-of-current", "other-repository"],
)
def test_a_non_empty_destination_that_was_not_staged_is_refused(
    layout: Layout, dest: str, cwd: str
) -> None:
    child = layout.victim / "child"
    child.mkdir()
    _ = layout.git("init", "-q", cwd=child)
    _ = (layout.victim / ".hidden").write_text("hidden\n")
    values = {"base": layout.base, "victim": layout.victim}
    before = layout.state()
    result = layout.stage(dest.format(**values), cwd=Path(cwd.format(**values)))
    assert result.returncode != 0
    assert result.stderr.startswith("stage_tree: ")
    assert "was not created by stage_tree.sh" in result.stderr
    assert layout.state() == before


def test_a_destination_that_is_a_file_is_refused(layout: Layout) -> None:
    target = layout.base / "afile"
    _ = target.write_text("precious\n")
    before = layout.state()
    result = layout.stage(target)
    assert result.returncode != 0
    assert "is not a directory" in result.stderr
    assert layout.state() == before


def test_an_empty_existing_destination_is_filled(layout: Layout) -> None:
    layout.dest.mkdir()
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert (layout.dest / "README.md").exists()


def test_a_partial_destination_from_an_interrupted_run_is_replaced(
    layout: Layout,
) -> None:
    (layout.dest / ".git").mkdir(parents=True)
    _ = (layout.dest / ".git/stage_tree").write_text("")
    _ = (layout.dest / "half.txt").write_text("partial\n")
    result = layout.stage()
    assert result.returncode == 0, result.stderr
    assert not (layout.dest / "half.txt").exists()
    assert tree_files(layout.dest) == tracked_and_untracked(layout)


@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_help_prints_usage_and_stages_nothing(layout: Layout, flag: str) -> None:
    before = layout.state()
    result = layout.stage(args=[flag])
    assert result.returncode == 0, result.stderr
    assert result.stdout.strip() == "usage: tools/stage_tree.sh DEST"
    assert layout.state() == before


@pytest.mark.parametrize("arg", ["-x", "--other", "-stage"])
def test_a_destination_starting_with_a_dash_is_refused(
    layout: Layout, arg: str
) -> None:
    before = layout.state()
    result = layout.stage(args=[arg])
    assert result.returncode != 0
    assert "starts with a dash" in result.stderr
    assert layout.state() == before


def test_a_dash_destination_works_with_a_path_prefix(layout: Layout) -> None:
    result = layout.stage("./-stage")
    assert result.returncode == 0, result.stderr
    assert (layout.base / "-stage/README.md").exists()
