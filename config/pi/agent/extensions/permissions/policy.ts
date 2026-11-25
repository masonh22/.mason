/**
 * Permission policy engine.
 *
 * Decides whether a tool call should be "allow"ed, "deny"ied, or whether
 * the user should be "ask"ed, based on a config loaded from
 * <agent-dir>/permissions.json (see config.ts for loading and schema
 * validation).
 *
 * Config shape (permissions.json):
 * {
 *   "mode": "write",                  // initial mode; defaults to the first
 *   "modes": {
 *     "read": {
 *       "default": "deny",            // fallback when no tool rule matches
 *       "tools": {
 *         "read": { "policy": "allow" },
 *         "mcp__github__*": { "policy": "ask" },  // "*" wildcard entry
 *         "bash": {
 *           "policy": "ask",
 *           "args": [
 *             { "path": "command", "regex": "^git ", "policy": "allow" }
 *           ]
 *         }
 *       }
 *     },
 *     "write": {
 *       "base": "read",               // inherit "read", then override below
 *       "tools": { "write": { "policy": "allow" } }
 *     },
 *     "deploy": {
 *       "base": "write",
 *       "hidden": true,                // not in the shift+tab cycle
 *       "tools": { ... }
 *     }
 *   },
 *   "groups": {
 *     "notion": {
 *       "description": "Notion workspace",
 *       "tools": ["mcp__notion__*"],    // patterns claiming tools for the group
 *       "read": ["mcp__notion__get_*"],  // read-classified tools
 *       "write": ["mcp__notion__update_*"], // write-classified tools
 *       "default": "ask"              // starting level: deny|ask|read|write|full
 *     }
 *   }
 * }
 *
 * Evaluation:
 * 1. If a group's "tools" patterns match the called tool name (first
 *    group in config order wins), the group's current level decides:
 *    deny blocks everything; ask prompts for everything; read allows
 *    read-classified tools and prompts for the rest; write allows
 *    read- and write-classified tools and prompts for the rest; full
 *    allows everything. Tools matching the group but no "read" or
 *    "write" pattern are undeclared.
 * 2. Otherwise the active mode applies. Exact "tools" entries win
 *    over wildcard entries (keys containing "*"); among wildcards the
 *    first match wins, with a mode's own entries tested before its
 *    base's. Within the matched entry, arg rules are tested in order
 *    and the first match wins.
 * 3. If no arg rule matches, the matched entry's policy applies.
 * 4. Otherwise the global default applies.
 *
 * Arg rules match a value at a dot path into the tool input ("command",
 * "options.recursive", ...). An empty path matches the whole input,
 * JSON-encoded. Non-string values are matched as JSON text. Regexes are
 * precompiled at load time (see config.ts). They are unanchored searches;
 * use ^ and $ to anchor.
 *
 * Modes can inherit from another mode via "base" (see resolveMode).
 * The active mode is chosen and cycled in index.ts; group levels are
 * switched there too.
 */

export type Decision = "allow" | "deny" | "ask";

/** Access level for a tool group, from most to least restrictive. */
export type GroupLevel = "deny" | "ask" | "read" | "write" | "full";

/** Group levels in cycle order. */
export const GROUP_LEVELS: readonly GroupLevel[] = ["deny", "ask", "read", "write", "full"];

/** Type guard for values that must be one of GROUP_LEVELS. */
export function isGroupLevel(value: unknown): value is GroupLevel {
  return typeof value === "string" && (GROUP_LEVELS as readonly string[]).includes(value);
}

/** Result of evaluating a tool call against the policy. */
export interface PolicyDecision {
  decision: Decision;
  /** Which rule produced this decision. Used in denial messages and audit entries. */
  rule: string;
}

/** Argument-level policy override. */
export interface ArgPolicy {
  /** Dot path into the tool input ("" matches the whole input as JSON). */
  path: string;
  /** Precompiled regular expression tested against the value. */
  regex: RegExp;
  policy: Decision;
}

/** Per-tool policy with optional argument-level overrides. */
export interface ToolPolicy {
  /** Fallback when no arg rule matches. */
  policy: Decision;
  /** Argument matchers, tested in order before the tool-level policy. */
  args?: ArgPolicy[];
}

/** Fully resolved policy for one mode. Tools are keyed by tool name. */
export interface PolicyConfig {
  default: Decision;
  tools: Record<string, ToolPolicy>;
  /** Wildcard "tools" entries, tested after exact names in order; first match wins. */
  wildcards: WildcardToolPolicy[];
}

/** A wildcard "tools" entry from a mode, kept in match order. */
export interface WildcardToolPolicy {
  /** Tool name pattern where "*" matches any characters. */
  pattern: string;
  /** The pattern, compiled at load time. */
  regex: RegExp;
  policy: ToolPolicy;
}

/** One mode as written in the config file, before its base is resolved. */
export interface ModeDefinition {
  /** Name of the mode this one inherits from. */
  base?: string;
  /** Fallback decision; inherits from the base when unset. */
  default?: Decision;
  /** Tool rules; see resolveMode for how these merge with the base's. */
  tools?: Record<string, ToolPolicy>;
  /** Exclude from shift+tab cycling; reachable only via "/permissions <name>". */
  hidden?: boolean;
}

/** Derived mode data, computed at load time. No serialized counterpart. */
export interface ModesConfig {
  /** All valid mode names in definition order. */
  order: string[];
  /** Non-hidden mode names; shift+tab cycles through these. */
  cycle: string[];
  /** Fully resolved policy per mode name. */
  resolved: Record<string, PolicyConfig>;
}

/**
 * Loaded permissions config; mirrors the serialized format: the
 * top-level "mode" and "modes" keys become config.mode and
 * config.modes, and "groups" becomes config.groups. The policies in
 * config.modes.resolved are fully resolved against their base chains.
 */
export interface PermissionsConfig {
  /** Mode a session starts in; the top-level "mode" key. */
  mode: string;
  modes: ModesConfig;
  groups: GroupsConfig;
}

/** A tool-name pattern ("*" matches any characters) with its compiled regex. */
export interface ToolPattern {
  pattern: string;
  regex: RegExp;
}

/** One tool group as written in the config file. */
export interface GroupDefinition {
  /** One-line description for menus and listings. */
  description?: string;
  /** Tool name patterns claiming tools for this group; "*" is a wildcard. */
  tools: ToolPattern[];
  /** Patterns for read-classified tools. */
  read: ToolPattern[];
  /** Patterns for write-classified tools. */
  write: ToolPattern[];
  /** Level a session starts at. */
  default: GroupLevel;
}

/** Parsed "groups" section of the config. */
export interface GroupsConfig {
  /** Group names in definition order; the first group claiming a tool wins. */
  order: string[];
  definitions: Record<string, GroupDefinition>;
}

/** Compile a tool-name pattern into an anchored regex; "*" matches any characters. */
export function toolPatternRegex(pattern: string): RegExp {
  const escaped = pattern
    .split("*")
    .map((part) => part.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"))
    .join(".*");
  return new RegExp(`^${escaped}$`);
}

function matchesAnyPattern(patterns: ToolPattern[], name: string): boolean {
  return patterns.some((p) => p.regex.test(name));
}

/** First group whose "tools" patterns claim the tool, in config order. */
function findGroup(toolName: string, groups: GroupsConfig): string | undefined {
  for (const name of groups.order) {
    if (matchesAnyPattern(groups.definitions[name].tools, toolName)) {
      return name;
    }
  }
  return undefined;
}

type ToolClass = "read" | "write" | "undeclared";

/** Classify a claimed tool against the group's "read"/"write" patterns. */
function classifyTool(toolName: string, group: GroupDefinition): ToolClass {
  if (matchesAnyPattern(group.read, toolName)) {
    return "read";
  }
  if (matchesAnyPattern(group.write, toolName)) {
    return "write";
  }
  return "undeclared";
}

/**
 * Evaluate a tool call against its group's current level; see the
 * module header for level semantics. Returns undefined when no group
 * claims the tool, so the caller falls back to the mode policy.
 */
export function evaluateGroup(
  toolName: string,
  groups: GroupsConfig,
  levels: Record<string, GroupLevel>,
): PolicyDecision | undefined {
  const name = findGroup(toolName, groups);
  if (name === undefined) {
    return undefined;
  }
  const group = groups.definitions[name];
  const level = levels[name] ?? group.default;
  const klass = classifyTool(toolName, group);
  const allowed =
    level === "full" ||
    (level === "read" && klass === "read") ||
    (level === "write" && klass !== "undeclared");
  const decision: Decision = level === "deny" ? "deny" : allowed ? "allow" : "ask";
  return { decision, rule: `group ${name} (${level}, ${klass})` };
}

/** Look up a value in the tool input by dot path ("" matches the whole input). */
function valueAtPath(input: unknown, path: string): unknown {
  if (path === "") {
    return input;
  }
  let current: unknown = input;
  for (const key of path.split(".")) {
    if (typeof current !== "object" || current === null) {
      return undefined;
    }
    current = (current as Record<string, unknown>)[key];
  }
  return current;
}

function argMatches(rule: ArgPolicy, input: unknown): boolean {
  const value = valueAtPath(input, rule.path);
  if (value === undefined) {
    return false;
  }
  const text = typeof value === "string" ? value : JSON.stringify(value);
  return rule.regex.test(text);
}

/** Short label for a rule's regex, used in denial messages and audit entries. */
function regexLabel(regex: RegExp): string {
  const source = regex.source;
  return source.length > 40 ? source.slice(0, 40) + "..." : source;
}

/**
 * Evaluate a tool call against a mode's resolved policy.
 *
 * toolName: the tool being called ("bash", "read", "write", "edit",
 *           "find", "grep", "ls", custom tools, MCP tools, ...).
 * input:    the tool arguments as given by the model (not yet validated).
 * policy:   the active mode's resolved policy.
 */
export function evaluate(toolName: string, input: unknown, config: PolicyConfig): PolicyDecision {
  const exact = config.tools[toolName];
  if (exact) {
    return evaluateEntry(input, exact, toolName);
  }
  const wildcard = config.wildcards.find((w) => w.regex.test(toolName));
  if (wildcard) {
    return evaluateEntry(input, wildcard.policy, wildcard.pattern);
  }
  return { decision: config.default, rule: "default" };
}

/** Apply one tool entry's arg rules, then its tool-level policy. */
function evaluateEntry(input: unknown, entry: ToolPolicy, rule: string): PolicyDecision {
  for (const arg of entry.args ?? []) {
    if (argMatches(arg, input)) {
      const path = arg.path === "" ? "<input>" : arg.path;
      return { decision: arg.policy, rule: `${rule}:${path}:${regexLabel(arg.regex)}` };
    }
  }
  return { decision: entry.policy, rule };
}

/**
 * Resolve a mode definition against its base chain.
 *
 * The mode's own "default" replaces the base's. For tools, the two
 * entries are merged per tool name:
 * - a tool only one side mentions is used as-is;
 * - a tool both sides mention keeps the mode's tool-level policy, and
 *   concatenates arg rules with the mode's own rules first. Arg rules
 *   are first-match-wins at evaluation time, so an own rule shadows an
 *   inherited rule that matches the same input. Arg rules cannot be
 *   removed, only shadowed.
 *
 * Wildcard entries (keys containing "*") are kept in a separate
 * ordered list, the mode's own entries before its base's, so an own
 * wildcard shadows an inherited one that matches the same tool.
 * Exact entries always win over wildcards at evaluation time.
 *
 * A mode without a base starts from ask-everything. Throws on unknown
 * bases and inheritance cycles.
 */
export function resolveMode(
  definitions: Record<string, ModeDefinition>,
  name: string,
  seen: string[] = [],
): PolicyConfig {
  const definition = definitions[name];
  if (!definition) {
    throw new Error(`mode "${name}" is not defined`);
  }
  const chain = [...seen, name];
  if (seen.includes(name)) {
    throw new Error(`mode inheritance cycle: ${chain.join(" -> ")}`);
  }
  const base: PolicyConfig =
    definition.base === undefined
      ? { default: "ask", tools: {}, wildcards: [] }
      : resolveMode(definitions, definition.base, chain);

  const tools: Record<string, ToolPolicy> = { ...base.tools };
  const ownWildcards: WildcardToolPolicy[] = [];
  for (const [tool, own] of Object.entries(definition.tools ?? {})) {
    if (tool.includes("*")) {
      ownWildcards.push({ pattern: tool, regex: toolPatternRegex(tool), policy: own });
      continue;
    }
    const inherited = base.tools[tool];
    tools[tool] =
      inherited === undefined
        ? own
        : { policy: own.policy, args: [...(own.args ?? []), ...(inherited.args ?? [])] };
  }
  return {
    default: definition.default ?? base.default,
    tools,
    wildcards: [...ownWildcards, ...base.wildcards],
  };
}

/** Maximum characters of formatted input shown in the approval prompt. */
const MAX_INPUT_CHARS = 2000;

/**
 * Human-readable summary of a tool call for the approval prompt.
 * Shows the input JSON, truncated if very long.
 */
export function describeInput(input: unknown): string {
  let text: string;
  if (input === undefined || input === null) {
    text = "(no arguments)";
  } else {
    try {
      text = JSON.stringify(input, null, 2);
    } catch {
      text = String(input);
    }
  }
  if (text.length > MAX_INPUT_CHARS) {
    text = text.slice(0, MAX_INPUT_CHARS) + "\n... (truncated)";
  }
  return text;
}
