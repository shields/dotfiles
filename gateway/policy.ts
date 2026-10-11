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
  body?: Buffer;
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

const repositoryName = /^[A-Za-z0-9_.-]+\/[A-Za-z0-9_.-]+$/;
const hexSha = /^(?:[0-9a-f]{40}|[0-9a-f]{64})$/;
const gitRoute =
  /^\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+)\.git\/(info\/refs|git-upload-pack|git-receive-pack)$/;
const restRoute = /^\/repos\/([A-Za-z0-9_.-]+)\/([A-Za-z0-9_.-]+)(?:\/|$)/;

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

export function parsePushCommands(
  body: Buffer,
  instanceId: string,
): { refs: string[]; bytes: number } | null {
  const refs: string[] = [];
  let offset = 0;
  let first = true;
  while (offset + 4 <= body.length) {
    const sizeText = body.toString("ascii", offset, offset + 4);
    if (!/^[0-9a-fA-F]{4}$/.test(sizeText)) {
      throw new PolicyError("invalid Git push packet");
    }
    const size = Number.parseInt(sizeText, 16);
    if (size === 0) {
      if (refs.length === 0) {
        throw new PolicyError("Git push has no ref updates");
      }
      return { refs, bytes: offset + 4 };
    }
    if (size < 5 || size > 65_520) {
      throw new PolicyError("invalid Git push packet length");
    }
    if (offset + size > body.length) {
      return null;
    }
    const line = body.toString("utf8", offset + 4, offset + size);
    const [command, capabilities] = line.split("\0", 2);
    if (first && capabilities?.split(/[\s\n]+/).includes("push-options")) {
      throw new PolicyError("Git push options are unsupported");
    }
    if (!first && capabilities !== undefined) {
      throw new PolicyError("unexpected Git push capabilities");
    }
    const match = /^([0-9a-f]+) ([0-9a-f]+) (refs\/heads\/[^\s\0]+)\n?$/.exec(
      command ?? "",
    );
    if (
      !match ||
      !hexSha.test(match[1] ?? "") ||
      !hexSha.test(match[2] ?? "") ||
      match[1]?.length !== match[2]?.length
    ) {
      throw new PolicyError("unsupported Git push command");
    }
    const ref = match[3] ?? "";
    if (
      !ref.startsWith(taskPrefix(instanceId)) ||
      ref.length === taskPrefix(instanceId).length
    ) {
      throw new PolicyError(
        "push destination is outside this VM's task namespace",
      );
    }
    if (/^0+$/.test(match[2] ?? "")) {
      throw new PolicyError("branch deletion is denied");
    }
    if (refs.includes(ref)) {
      throw new PolicyError("duplicate Git push destination");
    }
    refs.push(ref);
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

export function graphqlRepository(body: Buffer): string {
  if (body.length > 1_048_576) {
    throw new PolicyError("GraphQL request is too large");
  }
  let input: unknown;
  try {
    input = JSON.parse(body.toString("utf8")) as unknown;
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
    (!payload.variables ||
      typeof payload.variables !== "object" ||
      Array.isArray(payload.variables))
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
  const method = request.method.toUpperCase();
  if (host !== "github.com" && host !== "api.github.com") {
    throw new PolicyError("unsupported GitHub host");
  }
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
        return { repository, operation: "git.read", refs: [] };
      }
      if (route === "git-upload-pack" && method === "POST" && !url.search) {
        return { repository, operation: "git.read", refs: [] };
      }
      if (route === "git-receive-pack" && method === "POST" && !url.search) {
        if (!request.body) {
          throw new PolicyError("Git push commands were not inspected");
        }
        const parsed = parsePushCommands(request.body, instanceId);
        if (parsed === null) {
          throw new PolicyError("incomplete Git push commands");
        }
        return { repository, operation: "git.push", refs: parsed.refs };
      }
    }
    if (
      method === "GET" &&
      /^\/[^/]+\/[^/]+\/(releases\/download|archive)\//.test(url.pathname)
    ) {
      return { repository: null, operation: "public.read", refs: [] };
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
  if (match && method === "GET") {
    return {
      repository: normalizeRepository(`${match[1] ?? ""}/${match[2] ?? ""}`),
      operation: "api.read",
      refs: [],
    };
  }
  throw new PolicyError("unsupported GitHub API operation");
}

export function authorize(grant: Grant | null, operation: Operation): void {
  if (operation === "public.read") {
    return;
  }
  if (!grant?.read) {
    throw new PolicyError("repository is not granted to this VM");
  }
  if (operation === "git.push" && !grant.pushTaskBranches) {
    throw new PolicyError(
      "task-branch pushes are not granted for this repository",
    );
  }
}
