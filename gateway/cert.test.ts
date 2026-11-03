import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import {
  createPrivateKey,
  generateKeyPairSync,
  X509Certificate,
} from "node:crypto";
import {
  chmodSync,
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { afterAll, beforeAll, test } from "bun:test";
import { ensureCertificates, type Certificates } from "./cert.ts";

const files = ["ca.pem", "ca-key.pem", "github.pem", "github-key.pem"];
const serverAuth = "1.3.6.1.5.5.7.3.1";

const certModule = path.join(import.meta.dirname, "cert.ts");

function withDirectory(run: (directory: string) => void): void {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-cert-"));
  try {
    run(directory);
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
}

let issuedDirectory = "";

beforeAll(() => {
  issuedDirectory = mkdtempSync(path.join(tmpdir(), "vm-access-cert-issued-"));
  ensureCertificates(issuedDirectory);
});

afterAll(() => {
  rmSync(issuedDirectory, { recursive: true, force: true });
});

function withIssued(run: (directory: string) => void): void {
  withDirectory((directory) => {
    cpSync(issuedDirectory, directory, { recursive: true });
    run(directory);
  });
}

function openssl(...arguments_: string[]): void {
  const result = spawnSync("openssl", arguments_, { encoding: "utf8" });
  assert.equal(result.status, 0, result.stderr);
}

function realOpenssl(): string {
  const real = spawnSync("sh", ["-c", "command -v openssl"], {
    encoding: "utf8",
  }).stdout.trim();
  assert.notEqual(real, "");
  return real;
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
  const authority = new X509Certificate(readFileSync(certificates.ca));
  assert.ok(
    authority.checkPrivateKey(
      createPrivateKey(
        readFileSync(path.join(path.dirname(certificates.ca), "ca-key.pem")),
      ),
    ),
    "the CA key does not match the CA certificate",
  );
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

function listing(directory: string): string[] {
  return readdirSync(directory).toSorted((a, b) => a.localeCompare(b));
}

function refused(directory: string): Error {
  const names = listing(directory);
  const before = snapshot(directory, names);
  let thrown: unknown;
  try {
    ensureCertificates(directory);
  } catch (error) {
    thrown = error;
  }
  assert.ok(thrown instanceof Error, "ensureCertificates did not throw");
  assert.deepEqual(listing(directory), names);
  assert.deepEqual(snapshot(directory, names), before);
  return thrown;
}

function assertKeyUnparseable(directory: string): void {
  const error = refused(directory);
  assert.equal(
    error.message,
    `gateway TLS key ${path.join(directory, "github-key.pem")} cannot be parsed as a private key; remove it and ${path.join(directory, "github.pem")} to issue a new pair`,
  );
  assert.ok(error.cause instanceof Error);
}

function assertKeyMissing(directory: string): void {
  assert.equal(
    refused(directory).message,
    `gateway TLS key is missing; remove ${path.join(directory, "github.pem")} to issue a new pair`,
  );
}

test("a fresh directory gets a CA and a leaf that chains to it, with private keys", () => {
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

test("a valid CA and leaf are reused without rewriting any file", () => {
  withIssued((directory) => {
    const before = snapshot(directory);
    const first = ensureCertificates(directory);
    assert.deepEqual(ensureCertificates(directory), first);
    assert.deepEqual(snapshot(directory), before);
    assertConsistent(first);
    assertKeysPrivate(directory);
  });
});

test("a leaf inside the renewal window is reissued under the same CA", () => {
  withIssued((directory) => {
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

test("a missing leaf is issued from the existing CA", () => {
  withIssued((directory) => {
    const authority = snapshot(directory, ["ca.pem", "ca-key.pem"]);
    rmSync(path.join(directory, "github.pem"));
    const certificates = ensureCertificates(directory);
    assert.deepEqual(snapshot(directory, ["ca.pem", "ca-key.pem"]), authority);
    assertConsistent(certificates);
    assertKeysPrivate(directory);
  });
});

test("a leaf without its key is an error that names the file to remove and nothing is rewritten", () => {
  withIssued((directory) => {
    rmSync(path.join(directory, "github-key.pem"));
    assertKeyMissing(directory);
  });
});

test("an unparseable leaf key is an error that names the files to remove, and removing them issues a new pair under the same CA", () => {
  withIssued((directory) => {
    const authority = snapshot(directory, ["ca.pem", "ca-key.pem"]);
    const key = read(directory, "github-key.pem");
    for (const content of [key.slice(0, 400), "", "not a key\n"]) {
      writeFileSync(path.join(directory, "github-key.pem"), content);
      assertKeyUnparseable(directory);
    }
    rmSync(path.join(directory, "github-key.pem"));
    rmSync(path.join(directory, "github.pem"));
    const certificates = ensureCertificates(directory);
    assert.deepEqual(snapshot(directory, ["ca.pem", "ca-key.pem"]), authority);
    assertConsistent(certificates);
    assertKeysPrivate(directory);
  });
});

test("an unparseable leaf key is an error even when the leaf is due for renewal", () => {
  withIssued((directory) => {
    issueLeaf(directory, 1);
    assert.ok(daysLeft(directory) < 30);
    writeFileSync(path.join(directory, "github-key.pem"), "not a key\n");
    assertKeyUnparseable(directory);
  });
});

test("a key that is not the leaf's is replaced together with the leaf", () => {
  withIssued((directory) => {
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

test("half of a CA pair is an error that names the file to remove, and nothing is regenerated", () => {
  withIssued((directory) => {
    for (const [present, missing] of [
      ["ca-key.pem", "ca.pem"],
      ["ca.pem", "ca-key.pem"],
    ] as const) {
      const saved = read(directory, missing);
      rmSync(path.join(directory, missing));
      const names = files.filter((name) => name !== missing);
      const before = snapshot(directory, names);
      assert.throws(() => ensureCertificates(directory), {
        message: `gateway CA files are incomplete: ${present} exists without ${missing}; remove ${path.join(directory, present)} to issue a new CA`,
      });
      assert.deepEqual(snapshot(directory, names), before);
      assert.ok(!existsSync(path.join(directory, missing)));
      writeFileSync(path.join(directory, missing), saved);
    }
  });
});

test("deleting the CA files issues a new CA and a leaf that chains to it", () => {
  withIssued((directory) => {
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

test("a failed reissue keeps the previous pair and leaves no work files", () => {
  withIssued((directory) => {
    issueLeaf(directory, 1);
    writeFileSync(path.join(directory, "ca-key.pem"), "");
    const before = snapshot(directory);
    assert.throws(() => ensureCertificates(directory), /openssl failed/);
    assert.deepEqual(snapshot(directory), before);
    assert.deepEqual(strays(directory), []);
  });
});

test("an openssl that cannot run is an error and the pair is kept", () => {
  withIssued((directory) => {
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

test("a leaf that does not verify once it is issued is an error", () => {
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

test("a fresh directory whose path contains a dot keeps the serial file inside it", () => {
  withDirectory((parent) => {
    // LibreSSL's default serial file is the CA path cut at its first dot plus
    // ".srl". A relative path makes that dot the one in ".config", so the
    // stray file lands in the parent.
    const original = process.cwd();
    process.chdir(parent);
    try {
      const directory = path.join(".config", "vm-access");
      assertConsistent(ensureCertificates(directory));
      assert.ok(existsSync(path.join(directory, "ca.srl")));
      assert.deepEqual(readdirSync(parent), [".config"]);
    } finally {
      process.chdir(original);
    }
  });
});

test("a lock left by a crashed run is an error that names the lock, and nothing is rewritten", () => {
  withIssued((directory) => {
    issueLeaf(directory, 1);
    const lock = path.join(directory, ".lock");
    mkdirSync(lock);
    const names = listing(directory);
    const before = snapshot(directory);
    assert.throws(
      () => ensureCertificates(directory, 200),
      (error: Error) => error.message.includes(`remove ${lock}`),
    );
    assert.deepEqual(listing(directory), names);
    assert.deepEqual(snapshot(directory), before);
  });
});

test("a valid pair is returned without taking the lock", () => {
  withIssued((directory) => {
    mkdirSync(path.join(directory, ".lock"));
    assertConsistent(ensureCertificates(directory, 200));
  });
});

// Without the lock, the shim's waits make runs that do not exclude each other
// end with a leaf its CA did not sign, whatever their timing.
const recordingOpenssl = `#!${process.execPath}
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync } from "node:fs";
import path from "node:path";

const args = process.argv.slice(2);
const { REAL_OPENSSL: real, RECORDS: records, SECRETS: secrets } = process.env;
const sleep = (milliseconds) =>
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, milliseconds);
const claim = (name) => {
  for (let rank = 0; ; rank++) {
    try {
      mkdirSync(path.join(records, name + "-" + String(rank)));
      return rank;
    } catch (error) {
      if (error.code !== "EEXIST") throw error;
    }
  }
};
const waitFor = (condition) => {
  const deadline = Date.now() + 30_000;
  while (!condition()) {
    if (Date.now() > deadline) throw new Error("the other run never arrived");
    sleep(20);
  }
};
const unlocked = () => !existsSync(path.join(secrets, ".lock"));
const execute = () => {
  const status = spawnSync(real, args, { stdio: "inherit" }).status ?? 1;
  if (status !== 0) process.exit(status);
};

if (args[0] === "req" && args.includes("-x509")) {
  const rank = claim("authority");
  execute();
  if (rank > 0 && unlocked()) {
    waitFor(() => existsSync(path.join(records, "signed")));
  }
} else if (args[0] === "x509" && args.includes("-req")) {
  const rank = claim("signature");
  const ca = args[args.indexOf("-CA") + 1];
  const before = readFileSync(ca);
  execute();
  if (rank === 0) {
    mkdirSync(path.join(records, "signed"));
    if (unlocked()) waitFor(() => !readFileSync(ca).equals(before));
  }
} else {
  execute();
}
`;

const child = `
import { ensureCertificates } from ${JSON.stringify(certModule)};
const [directory, startAt] = process.argv.slice(1);
Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, Math.max(0, Number(startAt) - Date.now()));
process.stdout.write(JSON.stringify(ensureCertificates(directory)));
`;

interface Outcome {
  status: number | null;
  stdout: string;
  stderr: string;
}

function runChild(
  directory: string,
  startAt: number,
  environment: NodeJS.ProcessEnv,
): Promise<Outcome> {
  return new Promise((resolve, reject) => {
    const subprocess = spawn(
      process.execPath,
      ["--input-type=module", "-e", child, directory, String(startAt)],
      { env: environment },
    );
    let stdout = "";
    let stderr = "";
    subprocess.stdout.setEncoding("utf8").on("data", (chunk: string) => {
      stdout += chunk;
    });
    subprocess.stderr.setEncoding("utf8").on("data", (chunk: string) => {
      stderr += chunk;
    });
    subprocess.on("error", reject);
    subprocess.on("close", (status) => {
      resolve({ status, stdout, stderr });
    });
  });
}

test("overlapping first runs issue one CA and one leaf and all return them", async () => {
  const shims = mkdtempSync(path.join(tmpdir(), "vm-access-cert-shim-"));
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-cert-race-"));
  try {
    const records = path.join(shims, "records");
    mkdirSync(records);
    const shim = path.join(shims, "openssl");
    writeFileSync(shim, recordingOpenssl);
    chmodSync(shim, 0o755);
    const environment = {
      ...process.env,
      PATH: `${shims}${path.delimiter}${process.env.PATH ?? ""}`,
      REAL_OPENSSL: realOpenssl(),
      RECORDS: records,
      SECRETS: directory,
    };
    const startAt = Date.now() + 300;
    const outcomes = await Promise.all(
      Array.from({ length: 6 }, () =>
        runChild(directory, startAt, environment),
      ),
    );
    for (const outcome of outcomes) {
      assert.equal(outcome.status, 0, outcome.stderr);
    }
    assert.equal(new Set(outcomes.map((outcome) => outcome.stdout)).size, 1);
    assert.deepEqual(
      readdirSync(records)
        .filter((name) => name !== "signed")
        .toSorted((a, b) => a.localeCompare(b)),
      ["authority-0", "signature-0"],
    );
    const before = snapshot(directory);
    assertConsistent(ensureCertificates(directory));
    assert.deepEqual(snapshot(directory), before);
    assertKeysPrivate(directory);
    assert.deepEqual(strays(directory), []);
  } finally {
    rmSync(shims, { recursive: true, force: true });
    rmSync(directory, { recursive: true, force: true });
  }
}, 60_000);

test("the leaf key is renamed into place before the leaf certificate", () => {
  withIssued((directory) => {
    const cert = path.join(directory, "github.pem");
    const key = path.join(directory, "github-key.pem");
    const oldKey = read(directory, "github-key.pem");
    rmSync(cert);
    mkdirSync(cert);
    assert.throws(() => ensureCertificates(directory), { syscall: "rename" });
    assert.notEqual(readFileSync(key, "utf8"), oldKey);
    assert.deepEqual(strays(directory), []);
  });
});

test("a lock that cannot be created for a reason other than existing is an error", () => {
  withDirectory((directory) => {
    chmodSync(directory, 0o500);
    try {
      assert.throws(() => ensureCertificates(directory, 200), {
        code: "EACCES",
        syscall: "mkdir",
        path: path.join(directory, ".lock"),
      });
    } finally {
      chmodSync(directory, 0o700);
    }
    assert.deepEqual(readdirSync(directory), []);
  });
});
