import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  createPrivateKey,
  generateKeyPairSync,
  X509Certificate,
} from "node:crypto";
import {
  existsSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { ensureCertificates, type Certificates } from "./cert.ts";

const files = ["ca.pem", "ca-key.pem", "github.pem", "github-key.pem"];
const serverAuth = "1.3.6.1.5.5.7.3.1";

function withDirectory(run: (directory: string) => void): void {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-cert-"));
  try {
    run(directory);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
}

function openssl(...arguments_: string[]): void {
  const result = spawnSync("openssl", arguments_, { encoding: "utf8" });
  assert.equal(result.status, 0, result.stderr);
}

function read(directory: string, name: string): string {
  return readFileSync(path.join(directory, name), "utf8");
}

function snapshot(directory: string, names = files): Record<string, string> {
  return Object.fromEntries(
    names.flatMap((name) => [
      [name, read(directory, name)],
      [`${name} mtime`, String(statSync(path.join(directory, name)).mtimeMs)],
    ]),
  );
}

function assertConsistent(certificates: Certificates): void {
  const result = spawnSync(
    "openssl",
    ["verify", "-CAfile", certificates.ca, certificates.cert],
    { encoding: "utf8" },
  );
  assert.equal(result.status, 0, `${result.stdout}${result.stderr}`);
  const leaf = new X509Certificate(readFileSync(certificates.cert));
  assert.ok(
    leaf.checkPrivateKey(createPrivateKey(readFileSync(certificates.key))),
    "the leaf key does not match the leaf certificate",
  );
  assert.equal(leaf.subjectAltName, "DNS:github.com, DNS:api.github.com");
  assert.deepEqual(leaf.keyUsage, [serverAuth]);
  assert.equal(leaf.publicKey.asymmetricKeyDetails?.modulusLength, 3072);
}

function assertKeysPrivate(directory: string): void {
  for (const name of ["ca-key.pem", "github-key.pem"]) {
    assert.equal(
      statSync(path.join(directory, name)).mode & 0o777,
      0o600,
      name,
    );
  }
}

function strays(directory: string): string[] {
  return readdirSync(directory).filter(
    (name) => ![...files, "ca.srl"].includes(name),
  );
}

function daysLeft(directory: string): number {
  const leaf = new X509Certificate(read(directory, "github.pem"));
  return (leaf.validToDate.getTime() - Date.now()) / 86_400_000;
}

function issueLeaf(directory: string, days: number): void {
  const request = path.join(directory, "short.csr");
  openssl(
    "req",
    "-new",
    "-key",
    path.join(directory, "github-key.pem"),
    "-subj",
    "/CN=github.com",
    "-out",
    request,
  );
  openssl(
    "x509",
    "-req",
    "-in",
    request,
    "-CA",
    path.join(directory, "ca.pem"),
    "-CAkey",
    path.join(directory, "ca-key.pem"),
    "-CAserial",
    path.join(directory, "ca.srl"),
    "-CAcreateserial",
    "-days",
    String(days),
    "-out",
    path.join(directory, "github.pem"),
  );
  rmSync(request);
}

void test("a fresh directory gets a CA and a leaf that chains to it, with private keys", () => {
  withDirectory((directory) => {
    const certificates = ensureCertificates(directory);
    assert.deepEqual(certificates, {
      ca: path.join(directory, "ca.pem"),
      cert: path.join(directory, "github.pem"),
      key: path.join(directory, "github-key.pem"),
    });
    assertConsistent(certificates);
    assertKeysPrivate(directory);
    assert.ok(daysLeft(directory) > 360);
    assert.deepEqual(strays(directory), []);
  });
});

void test("a valid CA and leaf are reused without rewriting any file", () => {
  withDirectory((directory) => {
    const first = ensureCertificates(directory);
    const before = snapshot(directory);
    assert.deepEqual(ensureCertificates(directory), first);
    assert.deepEqual(snapshot(directory), before);
    assertConsistent(first);
    assertKeysPrivate(directory);
  });
});

void test("a leaf inside the renewal window is reissued under the same CA", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    issueLeaf(directory, 29);
    assert.ok(daysLeft(directory) < 30);
    const authority = snapshot(directory, ["ca.pem", "ca-key.pem"]);
    const certificates = ensureCertificates(directory);
    assert.deepEqual(snapshot(directory, ["ca.pem", "ca-key.pem"]), authority);
    assert.ok(daysLeft(directory) > 360);
    assertConsistent(certificates);
    assertKeysPrivate(directory);
  });
});

void test("a missing leaf is issued from the existing CA", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    const authority = snapshot(directory, ["ca.pem", "ca-key.pem"]);
    rmSync(path.join(directory, "github.pem"));
    const certificates = ensureCertificates(directory);
    assert.deepEqual(snapshot(directory, ["ca.pem", "ca-key.pem"]), authority);
    assertConsistent(certificates);
    assertKeysPrivate(directory);
  });
});

void test("a leaf without its key is an error and nothing is rewritten", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    rmSync(path.join(directory, "github-key.pem"));
    const names = ["ca.pem", "ca-key.pem", "github.pem"];
    const before = snapshot(directory, names);
    assert.throws(() => ensureCertificates(directory), /TLS key is missing/);
    assert.deepEqual(snapshot(directory, names), before);
    assert.ok(!existsSync(path.join(directory, "github-key.pem")));
  });
});

void test("a key that is not the leaf's is replaced together with the leaf", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    const { privateKey } = generateKeyPairSync("rsa", { modulusLength: 2048 });
    writeFileSync(
      path.join(directory, "github-key.pem"),
      privateKey.export({ type: "pkcs8", format: "pem" }),
      { mode: 0o600 },
    );
    assertConsistent(ensureCertificates(directory));
    assertKeysPrivate(directory);
  });
});

void test("half of a CA pair is an error and nothing is regenerated", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    for (const missing of ["ca.pem", "ca-key.pem"]) {
      const saved = read(directory, missing);
      rmSync(path.join(directory, missing));
      const names = files.filter((name) => name !== missing);
      const before = snapshot(directory, names);
      assert.throws(
        () => ensureCertificates(directory),
        /gateway CA files are incomplete/,
      );
      assert.deepEqual(snapshot(directory, names), before);
      assert.ok(!existsSync(path.join(directory, missing)));
      writeFileSync(path.join(directory, missing), saved);
    }
  });
});

void test("deleting the CA files issues a new CA and a leaf that chains to it", () => {
  withDirectory((directory) => {
    const first = ensureCertificates(directory);
    const authority = read(directory, "ca.pem");
    rmSync(first.ca);
    rmSync(path.join(directory, "ca-key.pem"));
    const second = ensureCertificates(directory);
    assert.deepEqual(second, first);
    assert.notEqual(read(directory, "ca.pem"), authority, "CA was not renewed");
    assertConsistent(second);
    assertKeysPrivate(directory);
  });
});

void test("a failed reissue keeps the previous pair and leaves no work files", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    issueLeaf(directory, 1);
    writeFileSync(path.join(directory, "ca-key.pem"), "");
    const before = snapshot(directory);
    assert.throws(() => ensureCertificates(directory), /openssl failed/);
    assert.deepEqual(snapshot(directory), before);
    assert.deepEqual(strays(directory), []);
  });
});

void test("an openssl that cannot run is an error and the pair is kept", () => {
  withDirectory((directory) => {
    ensureCertificates(directory);
    const before = snapshot(directory);
    const originalPath = process.env.PATH;
    process.env.PATH = directory;
    try {
      assert.throws(
        () => ensureCertificates(directory),
        /openssl x509 did not finish/,
      );
    } finally {
      process.env.PATH = originalPath;
    }
    assert.deepEqual(snapshot(directory), before);
  });
});

void test("a leaf that does not verify once it is issued is an error", () => {
  withDirectory((directory) => {
    openssl(
      "req",
      "-x509",
      "-newkey",
      "rsa:2048",
      "-nodes",
      "-keyout",
      path.join(directory, "ca-key.pem"),
      "-out",
      path.join(directory, "ca.pem"),
      "-subj",
      "/CN=cannot-sign",
      "-addext",
      "basicConstraints=critical,CA:TRUE",
      "-addext",
      "keyUsage=critical,digitalSignature",
    );
    assert.throws(
      () => ensureCertificates(directory),
      /does not verify against the gateway CA/,
    );
    assert.ok(!existsSync(path.join(directory, "github.pem")));
    assert.ok(!existsSync(path.join(directory, "github-key.pem")));
  });
});
