/**
 * Permission policy engine.
 *
 * Decides whether a tool call should be "allow"ed, "deny"ied, or whether
 * the user should be "ask"ed, based on a config loaded from
 * <agent-dir>/permissions.json (see config.ts for loading and schema
 * validation).
 *
 * Config shape:
 * {
 *   "default": "ask",                 // fallback when no tool rule matches
 *   "tools": {
 *     "read": { "policy": "allow" },
 *     "bash": {
 *       "policy": "ask",
 *       "args": [
 *         { "path": "command", "regex": "^git ", "policy": "allow" }
 *       ]
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
 * Planned (not yet implemented):
 * - Modes/profiles (e.g. read-only) that enable sets of tools, layered
 *   over a base mode, cycled with shift+tab.
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

/** Parsed and validated permissions config. Tools are keyed by tool name. */
export interface PolicyConfig {
  default: Decision;
  tools: Record<string, ToolPolicy>;
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
 * Evaluate a tool call against the config.
 *
 * toolName: the tool being called ("bash", "read", "write", "edit",
 *           "find", "grep", "ls", custom tools, MCP tools, ...).
 * input:    the tool arguments as given by the model (not yet validated).
 * config:   the loaded permissions config.
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
