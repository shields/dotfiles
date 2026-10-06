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

# lima/dev.yaml is checked by limactl itself, with its defaults filled in, so
# these tests need no YAML library. They skip where limactl is missing, as in
# the Linux image. They read the settings; only a running VM shows that the
# isolation works.

import ipaddress
import os
import re
import shutil
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
TEMPLATE = REPO / "lima" / "dev.yaml"
LIMACTL = shutil.which("limactl")

pytestmark = pytest.mark.skipif(
    LIMACTL is None,
    reason="limactl is installed on the Mac only, not in the Linux image",
)


@pytest.fixture(scope="module")
def lima_home(tmp_path_factory: pytest.TempPathFactory) -> Path:
    # Lima creates ~/.lima/_config on first use; a throwaway home keeps these
    # tests away from the real one.
    return tmp_path_factory.mktemp("lima-home")


def limactl(lima_home: Path, *args: str) -> str:
    result = subprocess.run(
        [LIMACTL or "limactl", "--tty=false", *args],
        env={**os.environ, "LIMA_HOME": str(lima_home)},
        capture_output=True,
        text=True,
        check=False,
        timeout=120,
    )
    assert result.returncode == 0, result.stderr
    return result.stdout


def query(lima_home: Path, expression: str) -> str:
    return limactl(lima_home, "template", "yq", str(TEMPLATE), expression).strip()


def test_limactl_validates_the_template(lima_home: Path) -> None:
    result = subprocess.run(
        [LIMACTL or "limactl", "validate", str(TEMPLATE)],
        env={**os.environ, "LIMA_HOME": str(lima_home)},
        capture_output=True,
        text=True,
        check=False,
        timeout=120,
    )
    assert result.returncode == 0, result.stderr
    assert "OK" in result.stderr


@pytest.mark.parametrize(
    ("expression", "expected"),
    [
        (".minimumLimaVersion", "2.2.1"),
        ('.images[0].location | test("debian-13-genericcloud")', "true"),
        (".vmType", "vz"),
        (".cpus", "6"),
        (".memory", "16GiB"),
        (".containerd.system", "false"),
        (".containerd.user", "false"),
        (".ssh.forwardAgent", "false"),
        (".propagateProxyEnv", "false"),
    ],
)
def test_settings(lima_home: Path, expression: str, expected: str) -> None:
    assert query(lima_home, expression) == expected


def test_the_base_is_the_image_list_without_mounts(lima_home: Path) -> None:
    assert re.search(
        r"^base: template:_images/debian-13$", TEMPLATE.read_text(), re.MULTILINE
    )
    assert query(lima_home, ".mounts | length") == "0"


def test_every_port_is_ignored(lima_home: Path) -> None:
    assert query(lima_home, ".portForwards | length") == "1"
    for field, expected in {
        ".guestIP": "0.0.0.0",  # noqa: S104 -- the address the rule matches, not a bind
        ".guestIPMustBeZero": "false",
        ".proto": "any",
        ".ignore": "true",
        '.guestPortRange | join("-")': "1-65535",
    }.items():
        assert query(lima_home, f".portForwards[0]{field}") == expected, field


def test_the_template_holds_no_secret_and_no_environment(lima_home: Path) -> None:
    filled = limactl(lima_home, "template", "copy", "--fill", str(TEMPLATE), "-")
    assert not re.search(
        r"token|secret|password(?!lessSudo)|api_key|credential", filled, re.IGNORECASE
    )
    assert query(lima_home, ".env") == "null"
    assert query(lima_home, ".param") == "null"
    assert query(lima_home, '[.provision[] | select(.mode == "data")] | length') == "0"


def test_provisioning_installs_packages_then_filters_egress(lima_home: Path) -> None:
    modes = query(lima_home, ".provision[].mode").splitlines()
    assert set(modes) == {"dependency", "system"}
    assert modes.count("dependency") == 1
    dependency = query(
        lima_home, '.provision[] | select(.mode == "dependency") | .script'
    )
    for package in ("nftables", "zsh", "git", "rsync"):
        assert package in dependency
    assert "command -v" in dependency
    assert "until apt-get -o DPkg::Lock::Timeout=300 update; do" in dependency


def firewall(lima_home: Path) -> list[str]:
    script = query(lima_home, '.provision[] | select(.mode == "system") | .script')
    start = script.index("table inet lima_isolation {")
    end = script.index("\nEOF", start)
    return [line.strip() for line in script[start:end].splitlines()]


def test_the_firewall_replaces_only_its_own_table_and_loads_at_boot(
    lima_home: Path,
) -> None:
    script = query(lima_home, '.provision[] | select(.mode == "system") | .script')
    assert "table inet lima_isolation\ndelete table inet lima_isolation\n" in script
    assert "flush ruleset" not in script
    assert "systemctl enable nftables.service" in script
    assert "systemctl restart nftables.service" in script


def test_the_firewall_rejects_the_host_and_the_private_ranges(lima_home: Path) -> None:
    rules = firewall(lima_home)
    (reject,) = [rule for rule in rules if rule.startswith("ip daddr {")]
    assert reject.endswith("} reject")
    listed = [
        ipaddress.ip_network(network.strip())
        for network in reject.removeprefix("ip daddr {")
        .removesuffix("} reject")
        .split(",")
    ]
    assert listed == [
        ipaddress.ip_network(network)
        for network in (
            "10.0.0.0/8",
            "172.16.0.0/12",
            "192.168.0.0/16",
            "100.64.0.0/10",
            "169.254.0.0/16",
        )
    ]
    for host in ("192.168.5.2", "192.168.64.1", "10.0.0.1", "169.254.169.254"):
        assert any(ipaddress.ip_address(host) in network for network in listed), host
    assert "ip6 daddr fc00::/7 reject" in rules


def test_the_firewall_allows_replies_loopback_dns_and_dhcp_before_it_rejects(
    lima_home: Path,
) -> None:
    rules = firewall(lima_home)
    reject = next(i for i, rule in enumerate(rules) if rule.startswith("ip daddr {"))
    for allowed in (
        'oifname "lo" accept',
        "ct state established,related accept",
        "ip daddr 192.168.5.2 udp dport 53 accept",
        "ip daddr 192.168.5.2 tcp dport 53 accept",
        "ip daddr 192.168.5.2 udp sport 68 udp dport 67 accept",
    ):
        assert rules.index(allowed) < reject, allowed
    allowed_hosts = [rule for rule in rules if "accept" in rule and "daddr" in rule]
    assert all(rule.startswith("ip daddr 192.168.5.2 ") for rule in allowed_hosts)


def test_the_readiness_probe_checks_the_filter_and_dns(lima_home: Path) -> None:
    assert query(lima_home, ".probes | length") == "1"
    assert query(lima_home, ".probes[0].mode") == "readiness"
    script = query(lima_home, ".probes[0].script")
    assert "nft list table inet lima_isolation" in script
    assert "getent hosts" in script
    assert "command -v zsh git rsync" in script
