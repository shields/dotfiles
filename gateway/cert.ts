import { spawnSync, type SpawnSyncReturns } from "node:child_process";
import { createPrivateKey, X509Certificate } from "node:crypto";
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

function paired(cert: string, key: string): boolean {
  return new X509Certificate(readFileSync(cert)).checkPrivateKey(
    createPrivateKey(readFileSync(key)),
  );
}

export function ensureCertificates(directory: string): Certificates {
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  const ca = path.join(directory, "ca.pem");
  const caKey = path.join(directory, "ca-key.pem");
  if (existsSync(ca) !== existsSync(caKey)) {
    throw new Error("gateway CA files are incomplete");
  }
  const cert = path.join(directory, "github.pem");
  const key = path.join(directory, "github-key.pem");
  if (!existsSync(ca)) {
    openssl(
      "req",
      "-x509",
      "-newkey",
      "rsa:3072",
      "-nodes",
      "-keyout",
      caKey,
      "-out",
      ca,
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
    chmodSync(caKey, 0o600);
  }
  if (existsSync(cert)) {
    if (!existsSync(key)) {
      throw new Error("gateway TLS key is missing");
    }
    // A leaf issued by any other CA is unexpired but fails every handshake, and
    // a key that is not the leaf's fails when the first listener starts, so the
    // chain and the key pair are checked as well as the dates.
    if (
      succeeds("x509", "-checkend", "2592000", "-noout", "-in", cert) &&
      succeeds("verify", "-CAfile", ca, cert) &&
      paired(cert, key)
    ) {
      return { ca, cert, key };
    }
  }
  // The pair is issued in a private directory and renamed into place once it
  // verifies, so a failed run keeps the previous pair and overlapping runs do
  // not delete each other's scratch files.
  const work = mkdtempSync(path.join(directory, ".issue-"));
  try {
    const csr = path.join(work, "github.csr");
    const extensions = path.join(work, "github.ext");
    const newKey = path.join(work, "github-key.pem");
    const newCert = path.join(work, "github.pem");
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
      ca,
      "-CAkey",
      caKey,
      "-CAserial",
      path.join(directory, "ca.srl"),
      "-CAcreateserial",
      "-out",
      newCert,
      "-days",
      "365",
      "-sha256",
      "-extfile",
      extensions,
    );
    const verified = run(["verify", "-CAfile", ca, newCert], 30_000);
    if (verified.status !== 0) {
      throw new Error(
        `gateway certificate does not verify against the gateway CA: ${`${verified.stdout}${verified.stderr}`.trim().slice(-300)}`,
      );
    }
    chmodSync(newKey, 0o600);
    renameSync(newKey, key);
    renameSync(newCert, cert);
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
  return { ca, cert, key };
}
