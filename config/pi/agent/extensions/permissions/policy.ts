/**
 * Permission policy engine.
 *
 * Decides whether a tool call should be "allow"ed, "deny"ied, or whether
 * the user should be "ask"ed. The default policy asks for every tool call.
 *
 * Planned (not yet implemented):
 * - JSON config with per-tool rules: allow / deny / ask.
 * - Fine-grained rules for bash (regex on the command) and other tools
 *   with rich arguments.
 * - Modes/profiles (e.g. read-only) that enable sets of tools, layered
 *   over a base mode, cycled with shift+tab.
 */

export type Decision = "allow" | "deny" | "ask";

/** Result of evaluating a tool call against the policy. */
export interface PolicyDecision {
  decision: Decision;
  /** Which rule produced this decision. Used in denial messages and audit entries. */
  rule: string;
}

/**
 * Evaluate a tool call against the current policy.
 *
 * toolName: the tool being called ("bash", "read", "write", "edit",
 *           "find", "grep", "ls", custom tools, MCP tools, ...).
 * input:    the tool arguments as given by the model (not yet validated).
 */
export function evaluate(toolName: string, _input: unknown): PolicyDecision {
  // Basic version: ask for every tool call, regardless of tool or input.
  void toolName;
  return { decision: "ask", rule: "default-ask" };
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
