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

"""A fake GitHub for tests: the device flow, refresh-token rotation, a little
REST and GraphQL (what gh asks for), and the smart-HTTP advertisement that
git ls-remote reads.

Tests import FakeGitHub. tests/test_limavm.zsh runs this file as a program with
a JSON scenario: python3 tests/fake_github.py CONFIG.json.
"""

import base64
import json
import signal
import ssl
import subprocess
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import TYPE_CHECKING, ClassVar, cast, final, override
from urllib.parse import parse_qs, urlsplit

if TYPE_CHECKING:
    from collections.abc import Mapping, Sequence
    from socket import socket

type Json = dict[str, object]

ACCESS_TTL = 28800
REFRESH_TTL = 15811200
GIT_SHA = "0123456789abcdef0123456789abcdef01234567"
ANY_REPOSITORY = "*"


def make_certificate(directory: Path) -> tuple[Path, Path]:
    """A self-signed certificate for 127.0.0.1, which TLS clients trust through
    SSL_CERT_FILE."""
    cert = directory / "cert.pem"
    key = directory / "key.pem"
    _ = subprocess.run(
        [
            "openssl",
            "req",
            "-x509",
            "-newkey",
            "rsa:2048",
            "-nodes",
            "-keyout",
            str(key),
            "-out",
            str(cert),
            "-days",
            "2",
            "-subj",
            "/CN=localhost",
            "-addext",
            "subjectAltName=DNS:localhost,IP:127.0.0.1",
        ],
        check=True,
        capture_output=True,
        timeout=60,
    )
    return cert, key


@final
class Request:
    def __init__(
        self,
        method: str,
        path: str,
        query: dict[str, list[str]],
        form: dict[str, str],
        headers: dict[str, str],
    ) -> None:
        self.method = method
        self.path = path
        self.query = query
        self.form = form
        self.headers = headers
        self.status = 0

    def as_json(self) -> Json:
        return {
            "method": self.method,
            "path": self.path,
            "form": self.form,
            "headers": self.headers,
            "status": self.status,
        }


@final
class FakeGitHub:
    """One fake GitHub on 127.0.0.1. Web, API and git share a port; the API is
    also served under /api/v3, as GitHub Enterprise Server does."""

    def __init__(
        self,
        *,
        client_id: str = "Iv-fake-client",
        repo_ids: Mapping[str, int] | None = None,
        device_polls: Sequence[str] = ("success",),
        device_interval: int = 0,
        device_expires_in: int = 900,
        access_ttl: int = ACCESS_TTL,
        refresh_ttl: int = REFRESH_TTL,
        oauth_scopes: str | None = None,
        public_viewer: bool = False,
        tls: tuple[Path, Path] | None = None,
        log_path: Path | None = None,
    ) -> None:
        self.client_id = client_id
        self.repo_ids = dict(repo_ids or {})
        self.device_polls = list(device_polls)
        self.device_interval = device_interval
        self.device_expires_in = device_expires_in
        self.access_ttl = access_ttl
        self.refresh_ttl = refresh_ttl
        self.oauth_scopes = oauth_scopes
        self.public_viewer = public_viewer
        self.tls = tls
        self.log_path = log_path
        self.refresh_delay = 0.0
        self.refresh_status = 0
        self.requests: list[Request] = []
        self._lock = threading.Lock()
        self._counter = 0
        self._access: dict[str, str] = {}
        self._refresh: dict[str, str] = {}
        self._polls = 0
        self._server: ThreadingHTTPServer | None = None
        self._thread: threading.Thread | None = None

    def __enter__(self) -> FakeGitHub:
        self.start()
        return self

    def __exit__(self, *exc: object) -> None:
        self.stop()

    def start(self) -> None:
        fake = self
        context: ssl.SSLContext | None = None
        if self.tls is not None:
            context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            context.load_cert_chain(*self.tls)

        class Server(ThreadingHTTPServer):
            # The handshake runs in the handler's thread, so a client that
            # never speaks TLS cannot stall the accept loop.
            @override
            def finish_request(
                self, request: socket | tuple[bytes, socket], client_address: object
            ) -> None:
                if context is not None:
                    request = context.wrap_socket(
                        cast("socket", request), server_side=True
                    )
                super().finish_request(request, client_address)  # type: ignore[arg-type]

        class Handler(_Handler):
            owner: ClassVar[FakeGitHub] = fake

        self._server = Server(("127.0.0.1", 0), Handler)
        self._thread = threading.Thread(target=self._server.serve_forever, daemon=True)
        self._thread.start()

    def stop(self) -> None:
        if self._server is not None:
            self._server.shutdown()
            self._server.server_close()
        if self._thread is not None:
            self._thread.join(timeout=10)

    @property
    def port(self) -> int:
        assert self._server is not None
        return self._server.server_address[1]

    @property
    def url(self) -> str:
        return f"{'https' if self.tls else 'http'}://127.0.0.1:{self.port}"

    @property
    def api_url(self) -> str:
        return f"{self.url}/api/v3"

    @property
    def host(self) -> str:
        return f"127.0.0.1:{self.port}"

    def issue(
        self, repository: str = ANY_REPOSITORY, *, access_ttl: int | None = None
    ) -> Json:
        """Make a token pair that is valid on this server, as the token endpoint
        answers it."""
        with self._lock:
            return self._issue_locked(repository, access_ttl)

    def _issue_locked(self, repository: str, access_ttl: int | None) -> Json:
        self._counter += 1
        access = f"ghu_fake-access-{self._counter}"
        refresh = f"ghr_fake-refresh-{self._counter}"
        self._access[access] = repository.lower()
        self._refresh[refresh] = repository.lower()
        return {
            "access_token": access,
            "expires_in": self.access_ttl if access_ttl is None else access_ttl,
            "refresh_token": refresh,
            "refresh_token_expires_in": self.refresh_ttl,
            "token_type": "bearer",
            "scope": "",
        }

    def expire_access(self, token: str) -> None:
        with self._lock:
            _ = self._access.pop(token, None)

    def valid_access_tokens(self) -> list[str]:
        with self._lock:
            return list(self._access)

    def requests_to(self, path: str) -> list[Request]:
        return [request for request in list(self.requests) if request.path == path]

    def refresh_requests(self) -> list[Request]:
        return [
            request
            for request in list(self.requests)
            if request.path == "/login/oauth/access_token"
            and request.form.get("grant_type") == "refresh_token"
        ]

    def record(self, request: Request) -> None:
        with self._lock:
            self.requests.append(request)
            if self.log_path is not None:
                with self.log_path.open("a") as log:
                    _ = log.write(json.dumps(request.as_json()) + "\n")

    def repository_for(self, token: str) -> str | None:
        with self._lock:
            return self._access.get(token)

    def next_poll(self) -> str:
        with self._lock:
            index = min(self._polls, len(self.device_polls) - 1)
            self._polls += 1
            return self.device_polls[index]

    def rotate(self, refresh_token: str) -> Json | None:
        with self._lock:
            repository = self._refresh.pop(refresh_token, None)
            if repository is None:
                return None
            self._access = {k: v for k, v in self._access.items() if v != repository}
            return self._issue_locked(repository, None)

    def repository_name(self, repository_id: str) -> str | None:
        for name, number in self.repo_ids.items():
            if str(number) == repository_id:
                return name
        return None


def parse_token(header: str) -> str | None:
    scheme, _, value = header.partition(" ")
    if scheme.lower() in {"bearer", "token"} and value:
        return value
    if scheme.lower() == "basic":
        try:
            decoded = base64.b64decode(value).decode()
        except ValueError:
            return None
        return decoded.partition(":")[2] or None
    return None


class _Handler(BaseHTTPRequestHandler):
    owner: ClassVar[FakeGitHub]
    protocol_version: str = "HTTP/1.1"

    @override
    def log_message(self, format: str, *args: object) -> None:
        pass

    def do_GET(self) -> None:
        self._serve()

    def do_POST(self) -> None:
        self._serve()

    def _serve(self) -> None:
        parts = urlsplit(self.path)
        length = int(self.headers.get("Content-Length") or 0)
        raw = self.rfile.read(length).decode() if length else ""
        form = {key: values[-1] for key, values in parse_qs(raw).items()}
        headers = {key.lower(): value for key, value in self.headers.items()}
        request = Request(
            self.command, parts.path, parse_qs(parts.query), form, headers
        )
        status, body, extra = self._route(request)
        request.status = status
        self.owner.record(request)
        payload = body if isinstance(body, bytes) else json.dumps(body).encode()
        self.send_response(status)
        content_type = extra.pop("Content-Type", "application/json")
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(payload)))
        for key, value in extra.items():
            self.send_header(key, value)
        self.end_headers()
        _ = self.wfile.write(payload)

    def _route(self, request: Request) -> tuple[int, Json | bytes, dict[str, str]]:
        path = request.path
        if path == "/login/device/code" and request.method == "POST":
            return self._device_code(request)
        if path == "/login/oauth/access_token" and request.method == "POST":
            return self._token(request)
        api = path.removeprefix("/api/v3") if path.startswith("/api/v3") else path
        if (
            api in {"", "/"}
            or api in {"/user", "/graphql"}
            or path == "/api/graphql"
            or api.startswith("/repos/")
        ):
            return self._api(
                request, "/graphql" if path == "/api/graphql" else api or "/"
            )
        if path.endswith("/info/refs"):
            return self._git(request)
        return 404, {"message": "Not Found"}, {}

    def _device_code(
        self, request: Request
    ) -> tuple[int, Json | bytes, dict[str, str]]:
        fake = self.owner
        if request.form.get("client_id") != fake.client_id:
            return (
                200,
                {
                    "error": "incorrect_client_credentials",
                    "error_description": "The client_id is not valid.",
                },
                {},
            )
        return (
            200,
            {
                "device_code": "fake-device-code",
                "user_code": "WDJB-MJHT",
                "verification_uri": f"{fake.url}/login/device",
                "expires_in": fake.device_expires_in,
                "interval": fake.device_interval,
            },
            {},
        )

    def _token(self, request: Request) -> tuple[int, Json | bytes, dict[str, str]]:
        fake = self.owner
        form = request.form
        if form.get("client_id") != fake.client_id:
            return (
                200,
                {
                    "error": "incorrect_client_credentials",
                    "error_description": "The client_id is not valid.",
                },
                {},
            )
        if form.get("grant_type") == "refresh_token":
            return self._refresh_grant(form)
        if form.get("grant_type") != "urn:ietf:params:oauth:grant-type:device_code":
            return (
                200,
                {"error": "unsupported_grant_type", "error_description": "bad grant"},
                {},
            )
        if form.get("device_code") != "fake-device-code":
            return (
                200,
                {
                    "error": "incorrect_device_code",
                    "error_description": "The device_code is not valid.",
                },
                {},
            )
        poll = fake.next_poll()
        errors = {
            "pending": {
                "error": "authorization_pending",
                "error_description": "Waiting for the user.",
            },
            "slow_down": {
                "error": "slow_down",
                "error_description": "Too fast.",
                "interval": fake.device_interval + 5,
            },
            "denied": {
                "error": "access_denied",
                "error_description": "The user said no.",
            },
            "expired": {
                "error": "expired_token",
                "error_description": "The device code expired.",
            },
            "disabled": {
                "error": "device_flow_disabled",
                "error_description": "Device flow is off.",
            },
        }
        if poll in errors:
            return 200, cast("Json", errors[poll]), {}
        repository = ANY_REPOSITORY
        if "repository_id" in form:
            name = fake.repository_name(form["repository_id"])
            if name is None:
                return (
                    200,
                    {
                        "error": "bad_repository",
                        "error_description": "No such repository id.",
                    },
                    {},
                )
            repository = name
        return 200, fake.issue(repository), {}

    def _refresh_grant(
        self, form: dict[str, str]
    ) -> tuple[int, Json | bytes, dict[str, str]]:
        fake = self.owner
        if fake.refresh_status:
            return fake.refresh_status, {"message": "Server Error"}, {}
        issued = fake.rotate(form.get("refresh_token", ""))
        if fake.refresh_delay:
            time.sleep(fake.refresh_delay)
        if issued is None:
            return (
                200,
                {
                    "error": "bad_refresh_token",
                    "error_description": "The refresh token is incorrect or expired.",
                },
                {},
            )
        return 200, issued, {}

    def _scopes(self) -> dict[str, str]:
        scopes = self.owner.oauth_scopes
        return {} if scopes is None else {"X-OAuth-Scopes": scopes}

    def _api(
        self, request: Request, api: str
    ) -> tuple[int, Json | bytes, dict[str, str]]:
        fake = self.owner
        token = parse_token(request.headers.get("authorization", ""))
        repository = fake.repository_for(token) if token else None
        extra = self._scopes()
        if api == "/":
            return 200, {}, extra
        if repository is None and not (
            fake.public_viewer and api in {"/user", "/graphql"}
        ):
            return 401, {"message": "Bad credentials"}, {}
        if api == "/user":
            return 200, {"login": "octo", "id": 1, "type": "User"}, extra
        if api == "/graphql":
            return 200, {"data": {"viewer": {"login": "octo"}}}, extra
        name = api.removeprefix("/repos/").lower()
        known = {key.lower(): key for key in fake.repo_ids}
        if name not in known:
            return 404, {"message": "Not Found"}, extra
        if repository not in {ANY_REPOSITORY, name}:
            return 403, {"message": "Resource not accessible by integration"}, extra
        return (
            200,
            {
                "full_name": known[name],
                "id": fake.repo_ids[known[name]],
                "permissions": {"admin": False, "push": True, "pull": True},
            },
            extra,
        )

    def _git(self, request: Request) -> tuple[int, Json | bytes, dict[str, str]]:
        fake = self.owner
        service = request.query.get("service", [""])[0]
        if service not in {"git-upload-pack", "git-receive-pack"}:
            return 404, {"message": "Not Found"}, {}
        token = parse_token(request.headers.get("authorization", ""))
        repository = fake.repository_for(token) if token else None
        if repository is None:
            return (
                401,
                b"",
                {
                    "WWW-Authenticate": 'Basic realm="GitHub"',
                    "Content-Type": "text/plain",
                },
            )
        name = (
            request.path.removesuffix("/info/refs")
            .removesuffix(".git")
            .strip("/")
            .lower()
        )
        if repository not in {ANY_REPOSITORY, name}:
            return (
                403,
                b"remote: Write access to repository not granted.\n",
                {"Content-Type": "text/plain"},
            )
        capabilities = "multi_ack thin-pack side-band ofs-delta agent=fake"
        first = f"{GIT_SHA} HEAD\0{capabilities}\n"
        lines = [first, f"{GIT_SHA} refs/heads/main\n"]
        banner = f"# service={service}\n"
        body = (
            f"{len(banner) + 4:04x}{banner}0000"
            + "".join(f"{len(line) + 4:04x}{line}" for line in lines)
            + "0000"
        )
        return (
            200,
            body.encode(),
            {"Content-Type": f"application/x-{service}-advertisement"},
        )


def main(argv: list[str]) -> int:
    config = cast("Json", json.loads(Path(argv[0]).read_text()))
    log = config.get("log")
    fake = FakeGitHub(
        client_id=cast("str", config.get("client_id", "Iv-fake-client")),
        repo_ids=cast("dict[str, int]", config.get("repo_ids", {})),
        device_polls=cast("list[str]", config.get("device_polls", ["success"])),
        device_interval=cast("int", config.get("device_interval", 0)),
        device_expires_in=cast("int", config.get("device_expires_in", 900)),
        access_ttl=cast("int", config.get("access_ttl", ACCESS_TTL)),
        log_path=Path(cast("str", log)) if log else None,
    )
    fake.start()
    _ = Path(cast("str", config["port_file"])).write_text(f"{fake.port}\n")
    done = threading.Event()

    def stop(*_: object) -> None:
        done.set()

    _ = signal.signal(signal.SIGTERM, stop)
    _ = signal.signal(signal.SIGINT, stop)
    _ = done.wait()
    fake.stop()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
