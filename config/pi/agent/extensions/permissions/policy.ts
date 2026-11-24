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
 *   }
 * }
 *
 * Evaluation:
 * 1. If the config has an entry for the called tool, its arg rules are
 *    tested in order; the first match wins.
 * 2. If no arg rule matches, the tool entry's policy applies.
 * 3. Otherwise the global default applies.
 *
 * Arg rules match a value at a dot path into the tool input ("command",
 * "options.recursive", ...). An empty path matches the whole input,
 * JSON-encoded. Non-string values are matched as JSON text. Regexes are
 * precompiled at load time (see config.ts). They are unanchored searches;
 * use ^ and $ to anchor.
 *
 * Modes can inherit from another mode via "base" (see resolveMode).
 * The active mode is chosen and cycled in index.ts.
 *
 * Planned (not yet implemented):
 * - Wildcard tool names (e.g. "mcp__*" for MCP tools).
 */

export type Decision = "allow" | "deny" | "ask";

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
 * config.modes. The policies in config.modes.resolved are fully
 * resolved against their base chains.
 */
export interface PermissionsConfig {
  /** Mode a session starts in; the top-level "mode" key. */
  mode: string;
  modes: ModesConfig;
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
  const entry = config.tools[toolName];
  if (entry) {
    for (const arg of entry.args ?? []) {
      if (argMatches(arg, input)) {
        const path = arg.path === "" ? "<input>" : arg.path;
        return { decision: arg.policy, rule: `${toolName}:${path}:${regexLabel(arg.regex)}` };
      }
    }
    return { decision: entry.policy, rule: toolName };
  }
  return { decision: config.default, rule: "default" };
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
      ? { default: "ask", tools: {} }
      : resolveMode(definitions, definition.base, chain);

  const tools: Record<string, ToolPolicy> = { ...base.tools };
  for (const [tool, own] of Object.entries(definition.tools ?? {})) {
    const inherited = base.tools[tool];
    tools[tool] =
      inherited === undefined
        ? own
        : { policy: own.policy, args: [...(own.args ?? []), ...(inherited.args ?? [])] };
  }
  return {
    default: definition.default ?? base.default,
    tools,
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
