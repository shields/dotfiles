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

import json
import os
import resource
import shutil
import subprocess
import sys
from pathlib import Path
from typing import cast

import pytest

REPO = Path(__file__).resolve().parents[1]
HOOK = REPO / ".codex/hooks/git_guard.py"
RULES = REPO / ".codex/rules"


@pytest.mark.parametrize(
    "command",
    [
        "git push origin main",
        "git -C /tmp/repo push origin main",
        "git -C '/tmp/my repo' -C nested push",
        "git -C/tmp/repo push",
        "git -c color.ui=false push",
        "git --exec-path=/tmp/git-tools push",
        "git --attr-source HEAD push",
        "git --attr-source=HEAD push",
        "git --git-dir=/tmp/repo/.git --work-tree /tmp/repo push",
        "/usr/bin/git -C /tmp/repo push",
        "/opt/homebrew/bin/git push",
        "/home/linuxbrew/.linuxbrew/bin/git push",
        "command git -C /tmp/repo push",
        "env -u GIT_DIR git push",
        "GIT_DIR=/tmp/repo/.git git push",
        "git status && git -C /tmp/repo push",
        "git status\ngit push",
        "if true; then git push; fi",
        "if false; then git status; else git push; fi",
        "while git push; do true; done",
        "for repo in a b; do git -C $repo push; done",
        "bash -lc 'git -C /tmp/repo push'",
        "git commit --no-verify -m message",
        "git commit -m message --no-verify",
        "git -C /tmp/repo commit -n -m message",
        "git commit -an -m message",
        "git commit -nm message",
        "git commit -mm --no-verify",
        "git commit -anmm --no-verify",
        "git -c core.hooksPath=/dev/null commit -m message",
        "git --config-env=core.hooksPath=DISABLED_HOOKS commit",
        "git --attr-source HEAD commit --no-verify",
        "git commit -m message; git push",
        "{ git -C /tmp/repo push; }",
        "time git -C /tmp/repo push",
        "cat <<'EOF'\ngit status\nEOF\ngit push",
        "git \\\n-C /tmp/repo push",
        "gh api repos/o/r/issues -X POST",
        "gh api -X DELETE repos/o/r",
        "gh api -XPATCH repos/o/r",
        "gh api -iX POST repos/o/r",
        "gh api --method POST repos/o/r/issues",
        "gh api --method=PUT repos/o/r/topics",
        "gh api repos/o/r --paginate --method DELETE",
        "gh api -H 'Accept: application/json' repos/o/r -X DELETE",
        "/usr/bin/gh api repos/o/r -X DELETE",
        "/home/linuxbrew/.linuxbrew/bin/gh api repos/o/r --method DELETE",
        "env GH_TOKEN=x gh api -X POST repos/o/r",
        "command gh api -X POST repos/o/r",
        "gh api -- repos/o/r -X",
        "bash -c 'gh api -X POST repos/o/r'",
        "echo done && gh api repos/o/r -X POST",
        "if gh api repos/o/r --method DELETE; then echo gone; fi",
        "gh api -X GET repos/o/r -X DELETE",
        "gh api --method GET --method POST repos/o/r",
        "gh api --method=get repos/o/r -XPOST",
        "gh api repos/o/r -X",
        "gh api --methods GET repos/o/r",
        'gh api -X "$M" repos/o/r',
        "gh api -X$M repos/o/r",
        "gh api -X GET repos/o/r $E",
        'gh api -X GET repos/o/r "$E"',
        'gh api -X GET "$E"',
        'gh api "-$F" GET repos/o/r -X GET',
        "gh api -X GET repos/{o,-XPOST}/r",
        "gh api -X GET repos/$o/$r/pulls",
        "gh api -X GET repos/o/r -f q=$q",
        'gh api -X GET repos/o/r -f"q=$q"',
        "gh api -X GET repos/o/r \\\n-XPOST",
        "gh api repos/o/r/issues -f title=x",
        "gh api repos/o/r/issues -F title=x",
        "gh api repos/o/r/issues --raw-field title=x",
        "gh api repos/o/r/issues --field=title=x",
        "gh api repos/o/r --input body.json",
        "gh api repos/o/r/issues -f title=x -q graphql",
        "gh api repos/o/r/issues -f title=graphql",
        'gh api "$endpoint" -f title=x',
        "gh api graphql -f query='mutation { addStar(input: {}) { id } }'",
        "gh api graphql -f query='{ viewer { login } }' -f query='mutation { a }'",
        "gh api graphql -F query=@query.graphql",
        "gh api graphql --input query.json",
        'gh api graphql -f query="$QUERY"',
        "gh api graphql -f query=$QUERY",
        "gh api graphql -f owner=x",
        'gh api graphql -f query="$operation { viewer { login } }"',
        'gh api graphql -f query="query { $field } mutation { a }"',
        'gh api graphql -f query="query { $f }" -f operationName=M',
        "git commit -m '>' --no-verify",
        "git -c user.name=T commit -m x '<' -n",
        "gh api -X GET repos/o/r '>' -XPOST",
        "gh api graphql -f",
        """gh api graphql -f "$name=x" -f query='{ viewer { login } }'""",
        "gh api graphql -f query='{ viewer { login } }' -F number=$n",
        "gh api graphql -f query='{ viewer { login } }' $E",
        "gh api 'graphql' -f query='mutation { a }'",
        "git -c user.name=T -c core.hooksPath=/dev/null commit -m message",
        "git -c core.hooksPath=/dev/null -c user.name=T commit -m message",
        "git -c user.name=T commit --no-verify -m message",
        "git -c user.name=T commit -n",
        "git -c user.name=T commit -qnm message",
        "git -c alias.commit='!true' commit",
        "git -c alias.ci='commit --no-verify' ci",
        "git -c user.namex=T commit -m message",
        "git -c user.name commit -m message",
        "git -c 'user.name =T' commit -m message",
        "git --config-env=user.name=NAME commit -m message",
        "git --config-env user.name=NAME commit -m message",
        "git -c user.name=T push",
        "git -c user.name=$name commit -m message",
        "git -cuser.name=$name commit -m message",
        "git -c user.name=T $options commit -m message",
        'git -c user.name=T "$options" commit -m message',
        "git -c user.name=T -$option commit -m message",
        "git -c user.name=T {-c,core.hooksPath=/dev/null} commit -m message",
        "git -c user.name=T -c{,ore.hooksPath=/dev/null} commit -m message",
        "git -C $repo -c user.name=T commit -m message",
        "git -c user.name=T -C ~/repo commit -m message",
        "git -c user.name=T commit $options",
        'git -c user.name=T commit "$options"',
        "git -c user.name=T commit -m message $options",
        'git -c user.name=T commit --message="$message"',
        "git -c user.name=T commit -m $message",
        "git -c user.name=T commit -F $file",
        "git -c user.name=T \\\n-c core.hooksPath=/dev/null commit",
        "git -c user.name=T -\\\nc core.hooksPath=/dev/null commit -m message",
        """bash -c 'git -c user.name=T -c core.hooksPath=/dev/null commit'""",
        'bash -c "git -c user.name=T -\\$option commit -m message"',
        """bash -c "git -c 'user.name=$name' commit -m message\"""",
        """bash -c "gh api -X GET 'repos/$repo'\"""",
        "git commit -m x 2>&1 --no-verify",
        "git -c user.name=T commit -m x 2>&1 --no-verify",
        "git commit -m x >&2 -n",
        "git commit -m x &>/dev/null --no-verify",
        "git commit -m x >|out --no-verify",
        "git -c core.hooksPath=/dev/null 2>&1 commit -m x",
        "gh api repos/o/r 2>&1 -X POST",
        "gh api -X GET repos/o/r 2>&1 -XPOST",
        "gh api -X >| /dev/null POST repos/o/r",
        ">/dev/null git push",
        "2>/dev/null git push",
        "2>&1 git push",
        "</dev/null git commit --no-verify -m x",
        "env >/dev/null git push",
        "git 2>/dev/null push",
        "bash >/dev/null -c 'git push'",
        'bash -c "git >/dev/null push; echo $HOME"',
        "=git push",
        "git -C . -c alias.ci='commit --no-verify' ci",
        "git --no-pager -c alias.ci='commit -n' ci",
        "git -calias.ci='commit -n' ci",
        "git commit --no-veri -m x",
        "git -c user.name=T commit --no-verif -m x",
        "git pu\\\nsh",
        "git commit --no-ver\\\nify -m x",
        "git -C $(pwd) push",
        "git -C $(git rev-parse --show-toplevel) push origin main",
        "gh api $(true) repos/o/r -X POST",
        "git -c user.name=T commit -F <(echo msg) -n",
        "x=$(pwd) git push",
        "git -c core.hooksPath=/dev/null $(echo commit) -m x",
        'git -c core.hooksPath=/dev/null "$sub" -m x',
        "git -C . -c user.name=T -c alias.ci='commit -n' ci",
        "git -C . -c ALIAS.ci='commit -n' ci",
        "git -C . --config-env=alias.ci=CMD ci",
        'K=alias.ci; git -c "$K=commit -n" ci',
        'git -c "$cfg" ci',
        "bash -c \"git -C . -c alias.ci='commit -n' ci\"",
        "git commit --no-verif -m message",
        "git -C /tmp/repo commit --no-verif -m message",
        "git commit --mess -m --no-verify",
        "git commit --me -t --no-verify",
        "git -c user.name=T commit --mess -m --no-verify",
        "git commit -Sc --no-verify -m message",
        "git commit --mess -m -n",
        "=git commit --no-verify -m x",
        "=gh api -X POST repos/o/r/issues",
        "env =git push",
        "time =git push",
        "=env git push",
        "=bash -c 'git push'",
        "zsh -c '=git push'",
        "timeout 30 gh api repos/o/r/issues -X POST -f title=x",
        "timeout -k 5 -s KILL 10 git push",
        "nice -n 5 gh api -X DELETE repos/o/r",
        "nohup gh api --method DELETE repos/o/r",
        "stdbuf -oL gh api -X PUT repos/o/r",
        "noglob gh api -X POST repos/o/r",
        "setsid git push",
        "sudo -u bob git push",
        "echo x | xargs gh api -X POST repos/o/r",
        "exec -a foo git push",
        "timeout 5 git -c core.hooksPath=/dev/null commit -m x",
        "gh api graphql -X GET -f query='mutation { addStar(input: {}) { id } }'",
        "gh api -X GET graphql -f query='mutation { a }'",
        "gh api graphql --method=HEAD -f query='mutation { a }'",
        "gh api graphql --method get -F query='mutation { a }'",
        "gh api graphql -X GET -F query=@query.graphql",
        "gh api graphql -X GET -f owner=x",
        'gh api graphql -X GET -f query="$QUERY"',
        "gh api -X GET graphql --input q.json",
        "gh api graphql --method HEAD --input q.json",
        "git commit -m x 2>&1 -n",
        "git commit 2>&1 --no-verify -m x",
        "git push 2>&1 | tee out",
        "git status && git push &> /dev/null",
        "cat <<EO\\\nF\nEOF\ngit push\nEO\nF",
        'git -c user.name=T commit -m "$(echo \'"\')" --no-verify -m "it\'s" # "',
        'git -c user.name=T -C "$(echo "a b")" commit -nm x',
        'git -c user.name=T commit "--no-\\\nverify"',
        'gh api graphql -f query="query { \\$x } mutation { a }"',
        'bash -c "git -c user.name=T commit -m \\$x --no-verify"',
        'gh api graphql -f query="query { $f }" -F n="$n" -f operationName=M',
        "gh api - graphql -f query='{ viewer { login } }'",
        "bash -o pipefail -c 'git push'",
        "bash -euo pipefail -c 'git push'",
        "bash -O extglob -c 'git push'",
        "zsh -o pipefail -c 'git push'",
        "bash -c -x 'git push'",
        "bash -c -- 'git push'",
        "sh -c -e 'git push'",
        "bash --rcfile /dev/null -c 'git push'",
        "bash --login -c 'git push'",
        "bash +o pipefail -c 'git commit --no-verify -m x'",
        "git -C $(pwd)/sub push",
        "git --git-dir=$(pwd)/.git push",
        "git -C $(git rev-parse --show-toplevel)/pkg push origin main",
        "git status && git -C $(pwd)/sub push",
        "bash -c 'git -C $(pwd)/sub push'",
        "git commit -m x $(true)--no-verify",
        "git commit -m x $(true)-n",
        "git -C $(pwd)$(echo /x) push",
        "git --git-dir $(echo /tmp/r/.)git push",
        "echo $(time case x in x) git push ;; esac)",
        "echo $(case x in x) git push ;; esac)",
        "$(true) git push",
        "env $(true) git push",
        "git commit -m x >(cat) --no-verify",
        'bash -c "$(true) git push"',
        'bash -c "echo $HOME; $(true) git push"',
        'sh -c "$(true) gh api -X POST repos/o/r/issues"',
        'bash -c "env $(true) git push"',
        "xargs -tn 1 git push",
        "xargs -0n 1 git push",
        "xargs --max-args 1 git push",
        "xargs -J % git push",
        "timeout -vk 1 5 git push",
        "timeout -fs KILL 5 git push",
        "env -vu FOO git push",
        "env -vC . git push",
        "sudo --user root git push",
        "git --shallow-file /dev/null push",
        "git -C . --shallow-file x push",
        "echo $(true)#; git push",
        "x=$(true)#; git push",
        "cat <(true)#; git push",
        "git $(true) push",
        "git -C . $(true) push",
        "git $(true) commit --no-verify -m x",
        "gh $(true) api -X POST repos/o/r/issues",
        "function f { git push; }; f",
        "coproc git push",
        "repeat 1 git push",
        "x[1]=1 git push",
        "x+=1 git push",
        "bash -c +x 'git push'",
        "bash -co pipefail 'git push'",
        "bash +c 'git push'",
        "zsh -O -c 'git push'",
        "bash -euo pipefail -c 'gh api -X POST repos/o/r'",
    ],
)
def test_git_hook_denies(command: str) -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    decision = response["hookSpecificOutput"]
    assert decision["permissionDecision"] == "deny"
    assert "failed" not in decision["permissionDecisionReason"]


@pytest.mark.parametrize(
    "command",
    [
        "git status",
        "git -C /tmp/repo status",
        "git -c color.ui=false diff",
        "git --attr-source HEAD diff",
        "git -- push origin main",
        "git -C /tmp/repo -- push",
        "git -- commit --no-verify",
        "git --exec-path /tmp/git-tools push",
        "git --exec-path push",
        "git log --oneline -n 10",
        "git commit -m message",
        "git commit -m 'git push; git commit --no-verify'",
        "git commit -m --no-verify",
        "git commit --message --no-verify",
        "git commit -mn",
        "git commit -F -n",
        "git commit -- -n",
        "printf '%s\\n' 'git push'",
        "printf '%s\\n' ';' git push",
        "# git push\ngit status",
        "rg 'git push' .codex",
        (
            "python3 - <<'PY'\n"
            'print("don\'t parse this as shell")\ngit push\nPY\ngit status'
        ),
        "cat <<-EOF\n\tgit push\n\tEOF\ngit status",
        "cat <<<'git push'\ngit status",
        "command -v git push",
        "command -V git push",
        "gh api repos/o/r",
        "gh api repos/o/r --paginate --jq '.[].name'",
        "gh api -H 'Accept: application/vnd.github+json' repos/o/r",
        "gh api repos/o/r -qX",
        "gh api graphql -f query='{ viewer { login } }'",
        "gh api 'graphql' -f query='{ viewer { login } }'",
        "gh api graphql -F owner=o -f query='{ viewer { login } }'",
        (
            "gh api graphql -f query='query($o: String!) "
            '{ repository(owner: $o, name: "r") { id } }\' -f o="$owner"'
        ),
        "gh api graphql --paginate -f query='{ __schema { mutationType { name } } }'",
        'gh api graphql -f query="query { repository(name: \\"$r\\") { id } }"',
        "gh api graphql -f query='{ viewer { login } }' > \"$TMPDIR/out.json\"",
        "gh api -X GET repos/o/r --jq .name > $TMPDIR/name.txt 2>&1",
        "git -c user.name=T commit -m message > $log 2>&1",
        'gh api graphql -f query="{ node(id: \\"$id\\") { id } }"',
        "git -C /tmp/repo -c core.commitGraph=false rev-list HEAD",
        "git -c user.name=T log --grep=commit",
        "gh api repos/o/r -X GET -f title=x",
        "gh api -X GET search/code -f q=term",
        'gh api -X GET search/code -f q="$term in:file"',
        "gh api --method GET repos/o/r --jq .name",
        "gh api --method=GET repos/o/r",
        "gh api --method get repos/o/r",
        "gh api -XGET repos/o/r",
        "gh api -iX GET repos/o/r",
        "gh api -X HEAD repos/o/r",
        "gh api -X GET -X HEAD repos/o/r",
        "gh api --method GET \"repos/$owner/${repo}/pulls\" --jq '.[].number'",
        'gh api -X GET repos/o/r -H "Accept: $accept" --jq="$filter"',
        "gh api $endpoint",
        "bash -c 'gh api -X GET repos/o/r'",
        "git -c user.name=T -c user.email=t@t.example commit -q --allow-empty -m x",
        "git -C /tmp/repo -c user.name=T -c user.email=t@example.com commit -m message",
        "git -c User.Name=T commit -m message",
        "git -cuser.email=t@example.com commit -m message",
        "git -c commit.gpgsign=false -c tag.gpgsign=false commit -m message",
        "git -c user.name='A B' commit -m message",
        'git -c user.name="$name" commit -m message',
        'git -C "$repo" -c user.name=T commit -qm "change $number"',
        "git -c user.name=T commit -F \"$file\" --author 'A <a@example.com>'",
        "git -c user.name=T commit -m message -- $paths",
        "git -c user.name=T add -A",
        "git -c user.name=T -C $repo status",
        "git -c color.ui=false log --oneline",
        "bash -c 'git -c user.name=T commit -m message'",
        "git -c core.commitGraph=false rev-list HEAD",
        "git -c color.ui=always log --grep=commit",
        "git -c core.pager=cat diff -- src/commit.py",
        "git -c commit.gpgsign=false -C $repo status",
        "git status 2>&1",
        "git commit -m message 2>&1",
        "gh api repos/o/r 2>&1 | head",
        "gh api 2>/dev/null graphql -f query='{ viewer { login } }'",
        "git commit -m 'a\\\nb'",
        "git -c core.pager=cat commit-graph verify",
        'git -c core.pager="$PAGER" log',
        "git -c core.pager=cat -C /tmp/alias.d log",
        "git commit --mess --no-verify",
        "git commit --mess -n",
        "git commit --message=--no-verify",
        "git commit -unormal -m message",
        "=git status",
        "=gh api -X GET repos/o/r",
        "echo =git push",
        'git commit -m "=git push"',
        "ls =git",
        "timeout 5 gh api -X GET repos/o/r",
        "timeout 30 gh api repos/o/r/issues --jq .",
        "nice -n 5 git status",
        "timeout 60 git -c protocol.version=0 ls-remote origin",
        "timeout 5 git -c user.name=T commit -m x",
        "xargs -n1 echo gh api -X POST",
        "timeout 5 echo git push",
        "gh api graphql -X GET -f query='{ viewer { login } }'",
        "gh api -X GET graphql -F owner=o -f query='{ viewer { login } }'",
        "gh api graphql --method=HEAD -f query='{ viewer { login } }'",
        "gh api -X GET graphql",
        "gh api -X GET repos/o/r --input body.json",
        ">/dev/null git status",
        "2>/dev/null git log --oneline",
        "echo 2>/dev/null git push",
        "git log -1>/dev/null",
        "git commit -m x > --no-verify",
        'gh api graphql -f query="query(\\$o: String!) { viewer { login } }" -f o=x',
        'git -c user.name=T commit -m "costs \\$5"',
        'git -c user.name=T commit -m "\\`code\\` span"',
        "gh api graphql -f query=@q.graphql",
        "gh api graphql -f query='{ viewer { login } }' -f operationName=Q",
        "bash script.sh -c 'git push'",
        "bash -- -c 'git push'",
        "bash -s",
        "bash -c",
        "bash -euo pipefail -c 'git status && git log'",
        "bash -o pipefail script.sh -c 'git push'",
        "bash -c true 'git push'",
        "bash file args",
        "bash -o pipefail -c 'gh api -X GET repos/o/r'",
        "git -C $(pwd)/sub status",
        "xargs -n1 echo git push",
        "xargs -t -n 1 git status",
        "sudo -u bob git status",
        "git --shallow-file /dev/null status",
        "echo a#; git status",
        "echo repeat 1 git push",
        "git -c url.https://alias.example.com/.insteadOf=x fetch",
        "git log --since=$(date +%F)",
        "echo $(case x in x) echo ok ;; esac)",
        "diff <(git -C $(pwd) status) <(git status)",
        "git status $(pwd) | cat",
        "git log $(git rev-parse HEAD)",
        "git diff >(cat) --stat",
        "git -C $(pwd) status",
        "gh pr list --search -X",
        "gh repo view o/r",
        "echo 'gh api -X POST repos/o/r'",
        "rg 'gh api --method' .codex",
        "printf '%s\\n' gh api -X",
    ],
)
def test_git_hook_allows(command: str) -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    assert result.stdout == ""


def run_hook(payload: dict[str, object]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(HOOK)],
        input=json.dumps(payload),
        text=True,
        capture_output=True,
        check=True,
        timeout=10,
    )


def test_hook_rejects_invalid_input() -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {}})
    assert (
        json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"] == "deny"
    )


def test_hook_ignores_other_tools() -> None:
    assert run_hook({"tool_name": "apply_patch", "tool_input": {}}).stdout == ""


WATCHDOG_SCRIPT = """\
import importlib.util
import sys

spec = importlib.util.spec_from_file_location("guard", sys.argv[1])
guard = importlib.util.module_from_spec(spec)
spec.loader.exec_module(guard)
guard.WALL_BUDGET = 0.3


def spin(script):
    while True:
        pass


guard.shell_reason = spin
guard.main()
"""


def test_hook_denies_when_it_runs_out_of_time() -> None:
    result = subprocess.run(
        [sys.executable, "-c", WATCHDOG_SCRIPT, str(HOOK)],
        input=json.dumps({"tool_name": "Bash", "tool_input": {"command": "true"}}),
        text=True,
        capture_output=True,
        check=True,
        timeout=60,
    )
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    decision = response["hookSpecificOutput"]
    assert decision["permissionDecision"] == "deny"
    assert "OutOfTime" in decision["permissionDecisionReason"]


def test_hook_reads_a_long_word_quickly() -> None:
    # Codex gives the hook 10 s and runs the command past that, so CPU time
    # measures it; a loaded host stretches wall-clock time without changing it.
    command = "git -c user.name=T commit -m " + "a" * 900_000 + " --no-verify"
    before = resource.getrusage(resource.RUSAGE_CHILDREN)
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    after = resource.getrusage(resource.RUSAGE_CHILDREN)
    cpu = (after.ru_utime - before.ru_utime) + (after.ru_stime - before.ru_stime)
    assert cpu < 2.0, f"the hook used {cpu:.1f} s of CPU"
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    assert "failed" not in response["hookSpecificOutput"]["permissionDecisionReason"]


@pytest.mark.parametrize("command", ["cat << ; git push", "cat <<\ngit push", "cat <<"])
def test_hook_rejects_missing_heredoc_delimiter(command: str) -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    decision = response["hookSpecificOutput"]
    assert decision["permissionDecision"] == "deny"
    assert "failed" in decision["permissionDecisionReason"]


def test_registered_hook_command() -> None:
    config = cast(
        "dict[str, dict[str, list[dict[str, object]]]]",
        json.loads((REPO / ".codex/hooks.json").read_text()),
    )
    hooks = cast("list[dict[str, str]]", config["hooks"]["PreToolUse"][0]["hooks"])
    command = hooks[0]["command"]
    result = subprocess.run(
        ["/bin/sh", "-c", command],
        input=json.dumps({"tool_name": "Bash", "tool_input": {"command": "git push"}}),
        env={**os.environ, "HOME": str(REPO)},
        text=True,
        capture_output=True,
        check=True,
        timeout=10,
    )
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    assert response["hookSpecificOutput"]["permissionDecision"] == "deny"


@pytest.mark.parametrize(
    ("command", "expected"),
    [
        (["git", "push", "origin", "main"], "forbidden"),
        (["/usr/bin/git", "push"], "forbidden"),
        (["/usr/local/bin/git", "push"], "forbidden"),
        (["/home/linuxbrew/.linuxbrew/bin/git", "push"], "forbidden"),
        (["gh", "repo", "delete", "example/repo"], "forbidden"),
        (["/usr/bin/gh", "repo", "delete", "example/repo"], "forbidden"),
        (["/opt/homebrew/bin/gh", "repo", "edit"], "forbidden"),
        (["/home/linuxbrew/.linuxbrew/bin/gh", "repo", "edit"], "forbidden"),
        (["gh", "repo", "archive"], "forbidden"),
        (["gh", "repo", "create"], "forbidden"),
        (["gh", "repo", "fork"], "forbidden"),
        (["gh", "repo", "rename"], "forbidden"),
        (["gh", "repo", "sync"], "forbidden"),
        (["gh", "repo", "unarchive"], "forbidden"),
        (["gh", "repo", "autolink", "create"], "forbidden"),
        (["gh", "repo", "autolink", "delete"], "forbidden"),
        (["/usr/bin/gh", "repo", "autolink", "create"], "forbidden"),
        (
            ["/home/linuxbrew/.linuxbrew/bin/gh", "repo", "autolink", "delete"],
            "forbidden",
        ),
        (["gh", "repo", "deploy-key", "add"], "forbidden"),
        (["gh", "repo", "deploy-key", "delete"], "forbidden"),
        (["/usr/bin/gh", "repo", "deploy-key", "add"], "forbidden"),
        (
            ["/home/linuxbrew/.linuxbrew/bin/gh", "repo", "deploy-key", "delete"],
            "forbidden",
        ),
        (["lefthook", "uninstall"], "forbidden"),
        (["git", "commit", "--no-verify"], "forbidden"),
        (["git", "commit", "-n"], "forbidden"),
        (["/usr/local/bin/git", "commit", "--no-verify"], "forbidden"),
        (["/home/linuxbrew/.linuxbrew/bin/git", "commit", "--no-verify"], "forbidden"),
        (["/home/linuxbrew/.linuxbrew/bin/git", "commit", "-n"], "forbidden"),
        (["gh", "repo", "view"], None),
        (["/usr/bin/gh", "repo", "view"], None),
        (["gh", "repo", "autolink", "list"], None),
        (["gh", "repo", "deploy-key", "list"], None),
        (["gh", "pr", "create"], None),
        (["/home/linuxbrew/.linuxbrew/bin/gh", "pr", "create"], None),
        (["git", "commit", "-m", "message"], None),
        (["/home/linuxbrew/.linuxbrew/bin/git", "commit", "-m", "message"], None),
        (["/home/linuxbrew/.linuxbrew/bin/git", "status"], None),
        (["lefthook", "run", "pre-commit"], None),
    ],
)
def test_execpolicy(command: list[str], expected: str | None) -> None:
    codex = shutil.which("codex")
    if codex is None:
        pytest.skip("Codex CLI is not installed")
    args = [codex, "execpolicy", "check"]
    for rules in sorted(RULES.glob("*.rules")):
        args.extend(["--rules", str(rules)])
    result = subprocess.run(
        [*args, "--", *command],
        text=True,
        capture_output=True,
        check=True,
        timeout=10,
    )
    response = cast("dict[str, object]", json.loads(result.stdout))
    assert response.get("decision") == expected
