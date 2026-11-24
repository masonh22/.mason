/**
 * Config loading for the permissions extension.
 *
 * Config file: <agent-dir>/permissions.json
 * (<agent-dir> comes from pi's getAgentDir(): $PI_CODING_AGENT_DIR with
 * tilde expansion, or ~/.config/pi/agent when unset.)
 *
 * A missing file is not an error: the DEFAULT_CONFIG applies (ask for
 * every tool call). Invalid parts of the file are reported as errors and
 * dropped; everything else still applies. If the file is missing or
 * unreadable, or cannot be parsed as JSON, the default config applies.
 *
 * See permissions.example.json next to this file for a documented example.
 */

import { getAgentDir } from "@earendil-works/pi-coding-agent";
import * as fs from "node:fs";
import * as path from "node:path";
import type { ArgPolicy, Decision, PolicyConfig, ToolPolicy } from "./policy.ts";

const DECISIONS: readonly Decision[] = ["allow", "ask", "deny"];

/** Used when the config file is missing or too broken to parse. */
export const DEFAULT_CONFIG: PolicyConfig = { default: "ask", tools: {} };

export function permissionsConfigPath(): string {
  return path.join(getAgentDir(), "permissions.json");
}

function parseDecision(value: unknown, errors: string[], where: string): Decision | undefined {
  if (typeof value === "string" && (DECISIONS as readonly string[]).includes(value)) {
    return value as Decision;
  }
  errors.push(`${where}: policy must be one of "allow", "ask", "deny"`);
  return undefined;
}

function parseArgPolicy(value: unknown, errors: string[], where: string): ArgPolicy | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    errors.push(`${where}: must be an object`);
    return undefined;
  }
  const entry = value as Record<string, unknown>;

  if (typeof entry.path !== "string") {
    errors.push(`${where}: "path" must be a string (use "" to match the whole input)`);
    return undefined;
  }
  if (typeof entry.regex !== "string" || entry.regex === "") {
    errors.push(`${where}: "regex" must be a non-empty string`);
    return undefined;
  }
  let regex: RegExp;
  try {
    regex = new RegExp(entry.regex);
  } catch (err) {
    errors.push(`${where}: invalid regex (${err instanceof Error ? err.message : String(err)})`);
    return undefined;
  }
  const policy = parseDecision(entry.policy, errors, where);
  if (!policy) {
    return undefined;
  }
  return { path: entry.path, regex, policy };
}

function parseToolPolicy(tool: string, value: unknown, errors: string[]): ToolPolicy | undefined {
  const where = `permissions.json: tools["${tool}"]`;
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    errors.push(`${where}: must be an object`);
    return undefined;
  }
  const entry = value as Record<string, unknown>;
  const policy = parseDecision(entry.policy, errors, where);

  // Args are parsed even when the policy is invalid, so the user gets
  // all errors in one pass.
  let args: ArgPolicy[] | undefined;
  if (entry.args !== undefined) {
    if (!Array.isArray(entry.args)) {
      errors.push(`${where}: "args" must be an array`);
    } else {
      const parsed: ArgPolicy[] = [];
      entry.args.forEach((arg, i) => {
        const match = parseArgPolicy(arg, errors, `${where}.args[${i}]`);
        if (match) {
          parsed.push(match);
        }
      });
      args = parsed;
    }
  }

  if (!policy) {
    return undefined;
  }
  return { policy, ...(args ? { args } : {}) };
}

export function loadPermissionsConfig(): { config: PolicyConfig; errors: string[] } {
  const file = permissionsConfigPath();

  let raw: string;
  try {
    raw = fs.readFileSync(file, "utf8");
  } catch {
    // Missing or unreadable file: fall back to the default policy.
    return { config: DEFAULT_CONFIG, errors: [] };
  }

  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return { config: DEFAULT_CONFIG, errors: [`permissions.json: invalid JSON (${message})`] };
  }

  if (typeof parsed !== "object" || parsed === null || Array.isArray(parsed)) {
    return { config: DEFAULT_CONFIG, errors: ["permissions.json: top level must be an object"] };
  }
  const root = parsed as Record<string, unknown>;

  const errors: string[] = [];
  let defaultDecision: Decision = "ask";
  if (root.default !== undefined) {
    const parsed = parseDecision(root.default, errors, `permissions.json: "default"`);
    if (parsed) {
      defaultDecision = parsed;
    }
  }

  const tools: Record<string, ToolPolicy> = {};
  if (root.tools !== undefined) {
    if (typeof root.tools !== "object" || root.tools === null || Array.isArray(root.tools)) {
      errors.push(`permissions.json: "tools" must be an object keyed by tool name`);
    } else {
      for (const [tool, value] of Object.entries(root.tools)) {
        const parsed = parseToolPolicy(tool, value, errors);
        if (parsed) {
          tools[tool] = parsed;
        }
      }
    }
  }

  return { config: { default: defaultDecision, tools }, errors };
}
