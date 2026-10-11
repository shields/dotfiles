import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import path from "node:path";

export interface Certificates {
  ca: string;
  cert: string;
  key: string;
}

function openssl(...arguments_: string[]): void {
  const result = spawnSync("openssl", arguments_, {
    encoding: "utf8",
    timeout: 60_000,
  });
  if (result.status !== 0) {
    throw new Error(`openssl failed: ${result.stderr.trim().slice(-300)}`);
  }
}

export function ensureCertificates(directory: string): Certificates {
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  const ca = path.join(directory, "ca.pem");
  const caKey = path.join(directory, "ca-key.pem");
  const cert = path.join(directory, "github.pem");
  const key = path.join(directory, "github-key.pem");
  if (existsSync(ca) !== existsSync(caKey)) {
    throw new Error("gateway CA files are incomplete");
  }
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
  }
  if (existsSync(cert)) {
    if (!existsSync(key)) {
      throw new Error("gateway TLS key is missing");
    }
    const result = spawnSync(
      "openssl",
      ["x509", "-checkend", "2592000", "-noout", "-in", cert],
      { encoding: "utf8", timeout: 30_000 },
    );
    if (result.status === 0) {
      return { ca, cert, key };
    }
  }
  const csr = path.join(directory, "github.csr");
  const extensions = path.join(directory, "github.ext");
  writeFileSync(
    extensions,
    "subjectAltName=DNS:github.com,DNS:api.github.com\nbasicConstraints=critical,CA:FALSE\nkeyUsage=critical,digitalSignature,keyEncipherment\nextendedKeyUsage=serverAuth\n",
    { mode: 0o600 },
  );
  try {
    openssl(
      "req",
      "-newkey",
      "rsa:3072",
      "-nodes",
      "-keyout",
      key,
      "-out",
      csr,
      "-subj",
      "/CN=github.com",
    );
    openssl(
      "x509",
      "-req",
      "-in",
      csr,
      "-CA",
      ca,
      "-CAkey",
      caKey,
      "-CAcreateserial",
      "-out",
      cert,
      "-days",
      "365",
      "-sha256",
      "-extfile",
      extensions,
    );
  } finally {
    rmSync(csr, { force: true });
    rmSync(extensions, { force: true });
  }
  return { ca, cert, key };
}
