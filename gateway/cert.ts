import { spawnSync, type SpawnSyncReturns } from "node:child_process";
import { createPrivateKey, X509Certificate, type KeyObject } from "node:crypto";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import path from "node:path";

export interface Certificates {
  ca: string;
  cert: string;
  key: string;
}

function run(arguments_: string[], timeout: number): SpawnSyncReturns<string> {
  const result = spawnSync("openssl", arguments_, {
    encoding: "utf8",
    timeout,
  });
  if (result.status === null) {
    throw new Error(
      `openssl ${arguments_[0] ?? ""} did not finish: ${result.error?.message ?? `killed by ${result.signal ?? "an unknown signal"}`}`,
    );
  }
  return result;
}

function openssl(...arguments_: string[]): void {
  const result = run(arguments_, 60_000);
  if (result.status !== 0) {
    throw new Error(`openssl failed: ${result.stderr.trim().slice(-300)}`);
  }
}

function succeeds(...arguments_: string[]): boolean {
  return run(arguments_, 30_000).status === 0;
}

function paired(cert: string, key: KeyObject): boolean {
  return new X509Certificate(readFileSync(cert)).checkPrivateKey(key);
}

function readLeafKey(key: string, cert: string): KeyObject {
  const pem = readFileSync(key);
  try {
    return createPrivateKey(pem);
  } catch (error) {
    throw new Error(
      `gateway TLS key ${key} cannot be parsed as a private key; remove it and ${cert} to issue a new pair`,
      { cause: error },
    );
  }
}

interface Paths {
  directory: string;
  ca: string;
  caKey: string;
  cert: string;
  key: string;
}

function pause(milliseconds: number): void {
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, milliseconds);
}

// Overlapping runs that issue would otherwise mix the files of different CAs
// or leaves. A run killed while it holds the lock leaves the lock behind, and
// the error names it.
function locked<T>(directory: string, timeout: number, work: () => T): T {
  const lock = path.join(directory, ".lock");
  const deadline = Date.now() + timeout;
  for (;;) {
    try {
      mkdirSync(lock, { mode: 0o700 });
      break;
    } catch (error) {
      if (
        !error ||
        typeof error !== "object" ||
        !("code" in error) ||
        error.code !== "EEXIST"
      ) {
        throw error;
      }
      if (Date.now() >= deadline) {
        throw new Error(
          `gateway certificates are being issued by another run; if none is running, remove ${lock}`,
          { cause: error },
        );
      }
      pause(100);
    }
  }
  try {
    return work();
  } finally {
    rmSync(lock, { recursive: true, force: true });
  }
}

// Files are generated in a private directory and renamed into place once they
// are complete, so a failed run keeps the previous files and a concurrent
// reader never sees a partly written one.
function inWorkDirectory(directory: string, work: (scratch: string) => void) {
  const scratch = mkdtempSync(path.join(directory, ".issue-"));
  try {
    work(scratch);
  } finally {
    rmSync(scratch, { recursive: true, force: true });
  }
}

function issueAuthority(scratch: string, { ca, caKey }: Paths): void {
  const newKey = path.join(scratch, "ca-key.pem");
  const newCa = path.join(scratch, "ca.pem");
  openssl(
    "req",
    "-x509",
    "-newkey",
    "rsa:3072",
    "-nodes",
    "-keyout",
    newKey,
    "-out",
    newCa,
    "-days",
    "3650",
    "-sha256",
    "-subj",
    "/CN=dotfiles-vm-access",
    "-addext",
    "basicConstraints=critical,CA:TRUE",
    "-addext",
    "keyUsage=critical,keyCertSign,cRLSign",
  );
  // LibreSSL leaves the mode of a key to the umask.
  chmodSync(newKey, 0o600);
  renameSync(newKey, caKey);
  renameSync(newCa, ca);
}

function validLeaf({ ca, cert, key }: Paths): Certificates | undefined {
  if (!existsSync(cert)) {
    return undefined;
  }
  if (!existsSync(key)) {
    throw new Error(
      `gateway TLS key is missing; remove ${cert} to issue a new pair`,
    );
  }
  const leafKey = readLeafKey(key, cert);
  // A leaf issued by any other CA is unexpired but fails every handshake, and
  // a key that is not the leaf's fails when the first listener starts, so the
  // chain and the key pair are checked as well as the dates.
  return succeeds("x509", "-checkend", "2592000", "-noout", "-in", cert) &&
    succeeds("verify", "-CAfile", ca, cert) &&
    paired(cert, leafKey)
    ? { ca, cert, key }
    : undefined;
}

function issueLeaf(scratch: string, paths: Paths): void {
  const csr = path.join(scratch, "github.csr");
  const extensions = path.join(scratch, "github.ext");
  const newKey = path.join(scratch, "github-key.pem");
  const newCert = path.join(scratch, "github.pem");
  writeFileSync(
    extensions,
    "subjectAltName=DNS:github.com,DNS:api.github.com\nbasicConstraints=critical,CA:FALSE\nkeyUsage=critical,digitalSignature,keyEncipherment\nextendedKeyUsage=serverAuth\n",
    { mode: 0o600 },
  );
  openssl(
    "req",
    "-newkey",
    "rsa:3072",
    "-nodes",
    "-keyout",
    newKey,
    "-out",
    csr,
    "-subj",
    "/CN=github.com",
  );
  // LibreSSL names the default serial file by cutting the CA path at its
  // first dot, which can land in the home directory.
  openssl(
    "x509",
    "-req",
    "-in",
    csr,
    "-CA",
    paths.ca,
    "-CAkey",
    paths.caKey,
    "-CAserial",
    path.join(paths.directory, "ca.srl"),
    "-CAcreateserial",
    "-out",
    newCert,
    "-days",
    "365",
    "-sha256",
    "-extfile",
    extensions,
  );
  const verified = run(["verify", "-CAfile", paths.ca, newCert], 30_000);
  if (verified.status !== 0) {
    throw new Error(
      `gateway certificate does not verify against the gateway CA: ${`${verified.stdout}${verified.stderr}`.trim().slice(-300)}`,
    );
  }
  chmodSync(newKey, 0o600);
  renameSync(newKey, paths.key);
  renameSync(newCert, paths.cert);
}

export function ensureCertificates(
  directory: string,
  lockTimeout = 120_000,
): Certificates {
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  const paths: Paths = {
    directory,
    ca: path.join(directory, "ca.pem"),
    caKey: path.join(directory, "ca-key.pem"),
    cert: path.join(directory, "github.pem"),
    key: path.join(directory, "github-key.pem"),
  };
  // A run that issues holds the lock for the whole time its files are in
  // flux, so anything short of a complete, valid set is judged again once the
  // lock is held.
  if (existsSync(paths.ca) && existsSync(paths.caKey)) {
    const found = validLeaf(paths);
    if (found) {
      return found;
    }
  }
  return locked(directory, lockTimeout, () => {
    const hasCa = existsSync(paths.ca);
    if (hasCa !== existsSync(paths.caKey)) {
      const present = hasCa ? "ca.pem" : "ca-key.pem";
      const missing = hasCa ? "ca-key.pem" : "ca.pem";
      throw new Error(
        `gateway CA files are incomplete: ${present} exists without ${missing}; remove ${path.join(directory, present)} to issue a new CA`,
      );
    }
    if (!hasCa) {
      inWorkDirectory(directory, (scratch) => {
        issueAuthority(scratch, paths);
      });
    }
    const found = validLeaf(paths);
    if (found) {
      return found;
    }
    inWorkDirectory(directory, (scratch) => {
      issueLeaf(scratch, paths);
    });
    return { ca: paths.ca, cert: paths.cert, key: paths.key };
  });
}
