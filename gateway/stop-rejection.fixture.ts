// Run by server.test.ts in its own process. bun test fails the running test
// on any unhandled rejection and never calls process handlers for it, so a
// rejection that must be observed has to happen outside the test runner.
import { once } from "node:events";
import https from "node:https";
import { mkdirSync, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { setImmediate as nextTurn } from "node:timers/promises";
import { AppAuth } from "./auth.ts";
import { Gateway } from "./server.ts";
import { State } from "./state.ts";

const [cert, key] = process.argv.slice(2);
if (!cert || !key) {
  throw new Error("usage: stop-rejection.fixture.ts CERT KEY");
}

const rejections: unknown[] = [];
const reported = Promise.withResolvers<undefined>();
process.on("unhandledRejection", (reason) => {
  rejections.push(reason instanceof Error ? reason.message : reason);
  reported.resolve(undefined);
});

const root = mkdtempSync(path.join(tmpdir(), "vm-access-rejection-"));
const state = new State(path.join(root, "state"));
try {
  const secrets = path.join(root, "secrets");
  mkdirSync(secrets);
  const auth = new AppAuth(secrets);
  const socket = path.join(root, "work.sock");
  state.register("lima:work", socket, "tester");
  state.update(
    "lima:work",
    0,
    { "shields/dotfiles": { read: true, pushTaskBranches: true } },
    "tester",
  );
  const events: string[] = [];
  const asked = Promise.withResolvers<undefined>();
  const resume = Promise.withResolvers<undefined>();
  auth.token = async (): Promise<string> => {
    asked.resolve(undefined);
    await resume.promise;
    events.push("token");
    return "ghu_secret";
  };
  state.finish = (): never => {
    throw new Error("audit write failed");
  };

  const createServer = https.createServer.bind(https);
  const listeners: https.Server[] = [];
  const closes: Promise<unknown>[] = [];
  https.createServer = ((...args: Parameters<typeof https.createServer>) => {
    const server = createServer(...args);
    listeners.push(server);
    server.on("request", (_request, response) => {
      closes.push(once(response, "close"));
    });
    return server;
  }) as typeof https.createServer;
  const gateway = new Gateway(state, auth, {
    cert,
    key,
    adminSocket: path.join(root, "admin.sock"),
  });
  await gateway.start();
  const listener = listeners[0];
  if (!listener) {
    throw new Error("the gateway did not create a listener");
  }

  const request = https.request({
    socketPath: socket,
    servername: "github.com",
    rejectUnauthorized: false,
    method: "GET",
    path: "/shields/dotfiles.git/info/refs?service=git-upload-pack",
    agent: false,
    headers: { host: "github.com" },
  });
  const hungUp = Promise.withResolvers<Error>();
  request.once("error", hungUp.resolve);
  request.end();
  await asked.promise;
  request.destroy();
  await hungUp.promise;
  await Promise.all(closes);

  const shutdown = (async () => {
    await gateway.stop();
    events.push("stopped");
  })();
  await once(listener, "close");
  await nextTurn();
  resume.resolve(undefined);
  await reported.promise;
  await shutdown;
  await nextTurn();
  process.stdout.write(JSON.stringify({ events, rejections }));
} finally {
  state.close();
  rmSync(root, { recursive: true, force: true });
}
