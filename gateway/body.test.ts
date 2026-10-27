import assert from "node:assert/strict";
import { afterEach, mock, spyOn, test } from "bun:test";
import { PushDenied } from "./policy.ts";
import { readBody } from "./server.ts";

afterEach(() => {
  mock.restore();
});

const id = "01234567-89ab-4cde-8fab-0123456789ab";
const old = "1".repeat(40);
const next = "2".repeat(40);

function packet(line: string): Buffer {
  const body = Buffer.from(line);
  return Buffer.concat([
    Buffer.from((body.length + 4).toString(16).padStart(4, "0")),
    body,
  ]);
}

function commands(count: number): Buffer[] {
  return Array.from({ length: count }, (_, index) =>
    packet(
      `${old} ${next} refs/heads/task/${id}/${String(index)}${index === 0 ? "\0report-status" : ""}\n`,
    ),
  );
}

function split(body: Buffer, size: number): Buffer[] {
  const chunks: Buffer[] = [];
  for (let offset = 0; offset < body.length; offset += size) {
    chunks.push(body.subarray(offset, offset + size));
  }
  return chunks;
}

function source(chunks: Buffer[]): {
  iterator: AsyncIterator<Buffer>;
  consumed: () => number;
} {
  let index = 0;
  return {
    iterator: {
      next: () =>
        Promise.resolve(
          index < chunks.length
            ? { done: false, value: chunks[index++] ?? Buffer.alloc(0) }
            : { done: true, value: undefined },
        ),
    },
    consumed: () => index,
  };
}

async function rest(iterator: AsyncIterator<Buffer>): Promise<Buffer> {
  const chunks: Buffer[] = [];
  for (;;) {
    const next = await iterator.next();
    if (next.done) {
      return Buffer.concat(chunks);
    }
    chunks.push(next.value);
  }
}

test("a push trickled in small writes costs work linear in its length", async () => {
  const prefix = Buffer.concat([...commands(1200), Buffer.from("0000")]);
  const body = Buffer.concat([prefix, Buffer.from("PACK")]);
  const chunks = split(body, 64);
  const decode = spyOn(TextDecoder.prototype, "decode");
  const concat = spyOn(Buffer, "concat");
  const allocate = spyOn(Buffer, "allocUnsafe");
  const { iterator } = source(chunks);
  const result = await readBody(iterator, undefined, 1_048_576, id);
  assert.ok(prefix.length > 100_000);
  assert.ok(
    result.buffered.equals(
      body.subarray(0, 64 * Math.ceil(prefix.length / 64)),
    ),
  );
  assert.ok(
    decode.mock.calls.length < 5 * chunks.length,
    `${String(decode.mock.calls.length)} decodes for ${String(chunks.length)} chunks`,
  );
  assert.ok(
    concat.mock.calls.length <= 1,
    `${String(concat.mock.calls.length)} concatenations`,
  );
  assert.ok(
    allocate.mock.calls.length < 20,
    `${String(allocate.mock.calls.length)} allocations for ${String(chunks.length)} chunks`,
  );
});

test("a push split at any point is read to the end of the chunk holding its flush packet", async () => {
  const flushed = Buffer.concat([...commands(3), Buffer.from("0000")]);
  const body = Buffer.concat([flushed, Buffer.from("PACKDATA")]);
  for (let size = 1; size <= body.length; size += 1) {
    const chunks = split(body, size);
    const { iterator, consumed } = source(chunks);
    const result = await readBody(iterator, undefined, 1_048_576, id);
    const last = Math.ceil(flushed.length / size);
    assert.equal(consumed(), last, `chunks of ${String(size)}`);
    assert.ok(result.buffered.equals(body.subarray(0, last * size)));
    assert.ok(result.remaining === iterator);
    assert.ok(
      Buffer.concat([result.buffered, await rest(iterator)]).equals(body),
    );
  }
  for (let at = 0; at <= body.length; at += 1) {
    const { iterator } = source([body.subarray(0, at), body.subarray(at)]);
    const result = await readBody(iterator, undefined, 1_048_576, id);
    assert.ok(
      Buffer.concat([result.buffered, await rest(iterator)]).equals(body),
      `split at ${String(at)}`,
    );
  }
});

test("a push is judged when the chunk completing it arrives", async () => {
  const body = Buffer.concat([
    ...commands(2),
    packet(`${old} ${next} refs/heads/main\n`),
    Buffer.from("0000PACK"),
  ]);
  const { iterator, consumed } = source(split(body, 7));
  await assert.rejects(
    readBody(iterator, undefined, 1_048_576, id),
    (error) =>
      error instanceof PushDenied && error.message.includes("outside this VM"),
  );
  assert.equal(consumed(), Math.ceil((body.length - 4) / 7));
});

test("a malformed packet is reported before the prefix limit", async () => {
  const body = Buffer.concat([Buffer.from("zzzz"), Buffer.alloc(100, 0x61)]);
  await assert.rejects(
    readBody(source([body]).iterator, undefined, 50, id),
    /invalid Git push packet$/,
  );
  const unfinished = Buffer.concat([...commands(5), packet("x".repeat(200))]);
  await assert.rejects(
    readBody(source([unfinished]).iterator, undefined, 50, id),
    /unsupported Git push command/,
  );
  const incomplete = Buffer.concat([
    ...commands(1),
    Buffer.from("0100"),
    Buffer.alloc(100, 0x61),
  ]);
  await assert.rejects(
    readBody(source(split(incomplete, 10)).iterator, undefined, 200, id),
    /request prefix is too large/,
  );
});

test("a lone flush packet is read to the end of the body and a later one is not a probe", async () => {
  const probe = source([Buffer.from("0000")]);
  const result = await readBody(probe.iterator, undefined, 1_048_576, id);
  assert.equal(result.buffered.toString(), "0000");
  await assert.rejects(
    readBody(
      source([Buffer.from("0000"), Buffer.from("PACK")]).iterator,
      undefined,
      1_048_576,
      id,
    ),
    /no ref updates/,
  );
});

test("an encoded body is refused before any of it is read", async () => {
  const { iterator, consumed } = source([Buffer.from("0000")]);
  await assert.rejects(
    readBody(iterator, "gzip", 1_048_576, id),
    /encoded request bodies are unsupported/,
  );
  assert.equal(consumed(), 0);
});

test("a body that is not a push is joined whole up to the limit", async () => {
  const body = Buffer.from("x".repeat(1000));
  const result = await readBody(
    source(split(body, 3)).iterator,
    undefined,
    1000,
  );
  assert.ok(result.buffered.equals(body));
  await assert.rejects(
    readBody(source(split(body, 3)).iterator, undefined, 999),
    /request prefix is too large/,
  );
  const empty = await readBody(source([]).iterator, undefined, 1000);
  assert.equal(empty.buffered.length, 0);
});
