import {
  Kind,
  getOperationAST,
  parse,
  type ArgumentNode,
  type FieldNode,
  type FragmentDefinitionNode,
  type SelectionNode,
  type ValueNode,
} from "graphql";

export type Profile = "read" | "task";
export interface Grant {
  read: boolean;
  pushTaskBranches: boolean;
}
export type Access = "anonymous" | "authenticated";
export type Operation =
  | "git.read"
  | "git.push"
  | "api.read"
  | "identity.read"
  | "graphql.read"
  | "public.read";

export interface RequestInfo {
  host: string;
  method: string;
  target: string;
  body?: Uint8Array;
}

export interface Classification {
  repository: string | null;
  operation: Operation;
  refs: string[];
}

export class PolicyError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PolicyError";
  }
}

export class PushDenied extends PolicyError {
  readonly report: Uint8Array<ArrayBuffer> | null;

  constructor(message: string, report: Uint8Array<ArrayBuffer> | null) {
    super(message);
    this.name = "PushDenied";
    this.report = report;
  }
}

const repositoryName = /^[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+$/;
const hexSha = /^(?:[0-9a-f]{40}|[0-9a-f]{64})$/;
const shallowLine = /^shallow (?:[0-9a-f]{40}|[0-9a-f]{64})\n?$/;
const gitRoute =
  /^\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+?)(?:\.git)?\/(info\/refs|git-upload-pack|git-receive-pack)$/;
const gitEndpoint = /\/(?:info\/refs|git-upload-pack|git-receive-pack)$/;
const webRoute = /^\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+)(?:\/|$)/;
const restRoute = /^\/repos\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+)(?:\/|$)/;
const repositoryIdLink =
  /<https:\/\/api\.github\.com\/repositories\/\d+(?=[/?>])/g;
const decoder = new TextDecoder();
const encoder = new TextEncoder();

export function denial(
  host: string,
  target: string,
  message: string,
  requestId: string | null,
): { contentType: string; body: string } {
  if (
    host === "github.com" &&
    gitEndpoint.test(target.split("?", 1)[0] ?? "")
  ) {
    return {
      contentType: "text/plain; charset=utf-8",
      body:
        requestId === null
          ? `${message}\n`
          : `${message}\nrequest ID: ${requestId}\n`,
    };
  }
  return {
    contentType: "application/json",
    body: JSON.stringify(
      requestId === null ? { message } : { message, request_id: requestId },
    ),
  };
}

function concat(parts: Uint8Array[]): Uint8Array<ArrayBuffer> {
  const joined = new Uint8Array(
    parts.reduce((length, part) => length + part.length, 0),
  );
  let offset = 0;
  for (const part of parts) {
    joined.set(part, offset);
    offset += part.length;
  }
  return joined;
}

function pktLine(data: Uint8Array): Uint8Array<ArrayBuffer> {
  return concat([
    encoder.encode((data.length + 4).toString(16).padStart(4, "0")),
    data,
  ]);
}

function pushReport(
  capabilities: string[],
  statuses: string[],
): Uint8Array<ArrayBuffer> | null {
  if (
    !capabilities.includes("report-status") &&
    !capabilities.includes("report-status-v2")
  ) {
    return null;
  }
  const flush = encoder.encode("0000");
  const report = concat([
    ...["unpack ok", ...statuses].map((line) =>
      pktLine(encoder.encode(`${line}\n`)),
    ),
    flush,
  ]);
  if (!capabilities.includes("side-band-64k")) {
    return report;
  }
  const packets: Uint8Array[] = [];
  for (let offset = 0; offset < report.length; offset += 65_515) {
    packets.push(
      pktLine(
        concat([Uint8Array.of(1), report.subarray(offset, offset + 65_515)]),
      ),
    );
  }
  return concat([...packets, flush]);
}

export function normalizeRepository(repository: string): string {
  if (
    !repositoryName.test(repository) ||
    repository.split("/").some((part) => part === "." || part === "..")
  ) {
    throw new PolicyError("repository must be OWNER/REPO");
  }
  return repository.toLowerCase();
}

export function taskPrefix(instanceId: string): string {
  if (!/^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(instanceId)) {
    throw new PolicyError("invalid VM identity");
  }
  return `refs/heads/task/${instanceId}/`;
}

// Before a push larger than http.postBuffer, git checks its credentials by
// posting a body that is only a flush packet.
function isPushProbe(body: Uint8Array): boolean {
  return body.length === 4 && decoder.decode(body) === "0000";
}

export function requireIdentityEncoding(
  contentEncoding: string | null | undefined,
): void {
  // GitHub acts on the decoded body, not the bytes the gateway inspects.
  if (
    contentEncoding !== undefined &&
    contentEncoding !== null &&
    contentEncoding.toLowerCase() !== "identity"
  ) {
    throw new PolicyError("encoded request bodies are unsupported");
  }
}

export function parsePushCommands(
  body: Uint8Array,
  instanceId: string,
): { refs: string[]; bytes: number } | null {
  const denials = new Map<string, string | null>();
  let capabilities: string[] = [];
  let offset = 0;
  let first = true;
  while (offset + 4 <= body.length) {
    const sizeText = decoder.decode(body.subarray(offset, offset + 4));
    if (!/^[0-9a-fA-F]{4}$/.test(sizeText)) {
      throw new PolicyError("invalid Git push packet");
    }
    const size = Number.parseInt(sizeText, 16);
    if (size === 0) {
      if (isPushProbe(body)) {
        return null;
      }
      if (denials.size === 0) {
        throw new PolicyError("Git push has no ref updates");
      }
      const denied = denials.values().find((reason) => reason !== null);
      if (denied) {
        throw new PushDenied(
          denied,
          pushReport(
            capabilities,
            Array.from(
              denials,
              ([ref, reason]) =>
                `ng ${ref} ${reason ?? "another ref update in this push is denied"}`,
            ),
          ),
        );
      }
      return { refs: [...denials.keys()], bytes: offset + 4 };
    }
    if (size < 5 || size > 65_520) {
      throw new PolicyError("invalid Git push packet length");
    }
    if (offset + size > body.length) {
      return null;
    }
    const line = decoder.decode(body.subarray(offset + 4, offset + size));
    // A push from a shallow clone lists its shallow commits before the first
    // command.
    if (first && shallowLine.test(line)) {
      offset += size;
      continue;
    }
    const [command = "", lineCapabilities, ...rest] = line.split("\0");
    if (rest.length > 0) {
      throw new PolicyError("unexpected Git push capabilities");
    }
    if (first) {
      capabilities = lineCapabilities?.split(/\s+/) ?? [];
      if (
        capabilities.some((capability) =>
          /^push-options(?:=|$)/.test(capability),
        )
      ) {
        throw new PolicyError("Git push options are unsupported");
      }
    } else if (lineCapabilities !== undefined) {
      throw new PolicyError("unexpected Git push capabilities");
    }
    const match = /^([0-9a-f]+) ([0-9a-f]+) (refs\/[^\s\0]+)\n?$/.exec(command);
    if (
      !match ||
      !hexSha.test(match[1] ?? "") ||
      !hexSha.test(match[2] ?? "") ||
      match[1]?.length !== match[2]?.length
    ) {
      throw new PolicyError("unsupported Git push command");
    }
    const ref = match[3] ?? "";
    if (denials.has(ref)) {
      throw new PolicyError("duplicate Git push destination");
    }
    denials.set(
      ref,
      !ref.startsWith(taskPrefix(instanceId)) ||
        ref.length === taskPrefix(instanceId).length
        ? `push destination is outside this VM's task namespace; allowed namespace: task/${instanceId}/*`
        : /^0+$/.test(match[2] ?? "")
          ? "branch deletion is denied"
          : null,
    );
    first = false;
    offset += size;
  }
  return null;
}

function value(
  node: ValueNode | undefined,
  variables: Record<string, unknown>,
): string {
  if (node?.kind === Kind.STRING) {
    return node.value;
  }
  if (node?.kind === Kind.VARIABLE) {
    const variable = variables[node.name.value];
    if (typeof variable === "string") {
      return variable;
    }
  }
  throw new PolicyError("GraphQL repository owner and name must be strings");
}

function argument(field: FieldNode, name: string): ArgumentNode | undefined {
  return field.arguments?.find((item) => item.name.value === name);
}

function rootFields(
  selections: readonly SelectionNode[],
  fragments: Map<string, FragmentDefinitionNode>,
  visiting: Set<string>,
): FieldNode[] {
  const fields: FieldNode[] = [];
  for (const selection of selections) {
    if (selection.kind === Kind.FIELD) {
      fields.push(selection);
    } else if (selection.kind === Kind.INLINE_FRAGMENT) {
      fields.push(
        ...rootFields(selection.selectionSet.selections, fragments, visiting),
      );
    } else {
      const name = selection.name.value;
      const fragment = fragments.get(name);
      if (!fragment || visiting.has(name)) {
        throw new PolicyError("unknown or recursive GraphQL fragment");
      }
      visiting.add(name);
      fields.push(
        ...rootFields(fragment.selectionSet.selections, fragments, visiting),
      );
      visiting.delete(name);
    }
  }
  return fields;
}

export function graphqlRepository(body: Uint8Array): string {
  if (body.length > 1_048_576) {
    throw new PolicyError("GraphQL request is too large");
  }
  let input: unknown;
  try {
    input = JSON.parse(decoder.decode(body)) as unknown;
  } catch {
    throw new PolicyError("invalid GraphQL JSON");
  }
  if (!input || typeof input !== "object" || Array.isArray(input)) {
    throw new PolicyError("invalid GraphQL request");
  }
  const payload = input as Record<string, unknown>;
  if (typeof payload.query !== "string") {
    throw new PolicyError("GraphQL query is missing");
  }
  if (
    payload.variables !== undefined &&
    payload.variables !== null &&
    (typeof payload.variables !== "object" || Array.isArray(payload.variables))
  ) {
    throw new PolicyError("invalid GraphQL variables");
  }
  let document;
  try {
    document = parse(payload.query, { maxTokens: 10_000 });
  } catch {
    throw new PolicyError("invalid GraphQL document");
  }
  const operation = getOperationAST(
    document,
    typeof payload.operationName === "string"
      ? payload.operationName
      : undefined,
  );
  if (operation?.operation !== "query") {
    throw new PolicyError("only GraphQL queries are allowed");
  }
  const fragments = new Map(
    document.definitions
      .filter((definition) => definition.kind === Kind.FRAGMENT_DEFINITION)
      .map((definition) => [definition.name.value, definition]),
  );
  const variables = (payload.variables ?? {}) as Record<string, unknown>;
  const repositories = new Set<string>();
  for (const field of rootFields(
    operation.selectionSet.selections,
    fragments,
    new Set(),
  )) {
    if (field.name.value === "__typename" || field.name.value === "rateLimit") {
      continue;
    }
    if (field.name.value === "viewer") {
      if (
        !field.selectionSet ||
        rootFields(field.selectionSet.selections, fragments, new Set()).some(
          (selection) =>
            selection.name.value !== "login" || selection.selectionSet,
        )
      ) {
        throw new PolicyError("unsupported GraphQL viewer field");
      }
      continue;
    }
    if (field.name.value !== "repository") {
      throw new PolicyError("unsupported GraphQL root field");
    }
    repositories.add(
      normalizeRepository(
        `${value(argument(field, "owner")?.value, variables)}/${value(argument(field, "name")?.value, variables)}`,
      ),
    );
  }
  if (repositories.size !== 1) {
    throw new PolicyError("GraphQL query must name exactly one repository");
  }
  return [...repositories][0] ?? "";
}

export function classify(
  request: RequestInfo,
  instanceId: string,
): Classification {
  const host = request.host.toLowerCase();
  if (host !== "github.com" && host !== "api.github.com") {
    throw new PolicyError("unsupported GitHub host");
  }
  const method = request.method.toUpperCase();
  let url: URL;
  try {
    url = new URL(request.target, `https://${host}`);
  } catch {
    throw new PolicyError("invalid request target");
  }
  if (url.host !== host || !request.target.startsWith("/")) {
    throw new PolicyError("request target must stay on its GitHub host");
  }
  if (host === "github.com") {
    const match = gitRoute.exec(url.pathname);
    if (match) {
      const repository = normalizeRepository(
        `${match[1] ?? ""}/${match[2] ?? ""}`,
      );
      const route = match[3];
      if (
        route === "info/refs" &&
        method === "GET" &&
        (url.search === "?service=git-upload-pack" ||
          url.search === "?service=git-receive-pack")
      ) {
        return {
          repository,
          operation:
            url.search === "?service=git-receive-pack"
              ? "git.push"
              : "git.read",
          refs: [],
        };
      }
      if (route === "git-upload-pack" && method === "POST" && !url.search) {
        return { repository, operation: "git.read", refs: [] };
      }
      if (route === "git-receive-pack" && method === "POST" && !url.search) {
        if (!request.body) {
          throw new PolicyError("Git push commands were not inspected");
        }
        if (isPushProbe(request.body)) {
          return { repository, operation: "git.push", refs: [] };
        }
        const parsed = parsePushCommands(request.body, instanceId);
        if (parsed === null) {
          throw new PolicyError("incomplete Git push commands");
        }
        return { repository, operation: "git.push", refs: parsed.refs };
      }
      throw new PolicyError("unsupported GitHub Git operation");
    }
    const web = webRoute.exec(url.pathname);
    if (web && (method === "GET" || method === "HEAD")) {
      return {
        repository: normalizeRepository(`${web[1] ?? ""}/${web[2] ?? ""}`),
        operation: "public.read",
        refs: [],
      };
    }
    throw new PolicyError("unsupported GitHub web operation");
  }
  if (
    url.pathname === "/__vm_access__/health" &&
    method === "GET" &&
    !url.search
  ) {
    return { repository: null, operation: "public.read", refs: [] };
  }
  if (
    url.pathname === "/graphql" &&
    method === "POST" &&
    !url.search &&
    request.body
  ) {
    return {
      repository: graphqlRepository(request.body),
      operation: "graphql.read",
      refs: [],
    };
  }
  if (url.pathname === "/user" && method === "GET" && !url.search) {
    return { repository: null, operation: "identity.read", refs: [] };
  }
  const match = restRoute.exec(url.pathname);
  if (match && (method === "GET" || method === "HEAD")) {
    return {
      repository: normalizeRepository(`${match[1] ?? ""}/${match[2] ?? ""}`),
      operation: "api.read",
      refs: [],
    };
  }
  throw new PolicyError("unsupported GitHub API operation");
}

// GitHub's pagination links address the repository by ID, which classify
// rejects. Location stays as is: GitHub redirects a renamed repository's old
// name to its ID, so rewriting a redirect back to the requested name loops.
export function rewriteLinks(
  link: string,
  classification: Classification,
): string {
  return classification.operation !== "api.read" ||
    classification.repository === null
    ? link
    : link.replaceAll(
        repositoryIdLink,
        `<https://api.github.com/repos/${classification.repository}`,
      );
}

export function authorize(grant: Grant | null, operation: Operation): Access {
  if (operation === "public.read") {
    return "anonymous";
  }
  if (!grant?.read) {
    if (operation === "git.read" || operation === "api.read") {
      return "anonymous";
    }
    throw new PolicyError("repository is not granted to this VM");
  }
  if (operation === "git.push" && !grant.pushTaskBranches) {
    throw new PolicyError(
      "task-branch pushes are not granted for this repository",
    );
  }
  return "authenticated";
}
