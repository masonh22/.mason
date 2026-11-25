/**
 * Config loading for the permissions extension.
 *
 * Config file: <agent-dir>/permissions.json
 * (<agent-dir> comes from pi's getAgentDir(): $PI_CODING_AGENT_DIR with
 * tilde expansion, or ~/.config/pi/agent when unset.)
 *
 * The file defines a "modes" object keyed by mode name. Each mode
 * has an optional "base" (another mode it inherits from), a
 * "default" decision, per-tool rules, and a "hidden" flag that keeps
 * it out of the shift+tab cycle (reachable only via "/permissions
 * <name>"). A mode starts from its base's resolved policy; its own
 * "default" and tool entries replace the base's. The active mode
 * starts at the top-level "mode" name, or the first defined mode,
 * and is cycled in index.ts.
 *
 * A "groups" object defines named tool groups (typically MCP
 * servers); see policy.ts for patterns, levels, and evaluation
 * order. Groups claim tools before the active mode sees them, and
 * are switched in index.ts.
 *
 * A missing file is not an error: the DEFAULT_CONFIG applies (built-in
 * "read" and "write" modes). Invalid parts of the file are reported as
 * errors and dropped; everything else still applies. If the file is
 * missing or unreadable, cannot be parsed as JSON, or defines no
 * usable modes, the default config applies.
 *
 * See permissions.example.json next to this file for a documented example.
 */

import { getAgentDir } from "@earendil-works/pi-coding-agent";
import * as fs from "node:fs";
import * as path from "node:path";
import {
  GROUP_LEVELS,
  isGroupLevel,
  resolveMode,
  toolPatternRegex,
  type ArgPolicy,
  type Decision,
  type GroupDefinition,
  type GroupsConfig,
  type ModeDefinition,
  type ModesConfig,
  type PermissionsConfig,
  type PolicyConfig,
  type ToolPattern,
  type ToolPolicy,
} from "./policy.ts";

const DECISIONS: readonly Decision[] = ["allow", "ask", "deny"];

/** Groups used when the config defines none. */
const EMPTY_GROUPS: GroupsConfig = { order: [], definitions: {} };

function bashArg(policy: Decision, regex: RegExp): ArgPolicy {
  return { path: "command", regex, policy };
}

/** Arg rules for bash in the default modes. Denies come first and
 * ask-guards second: arg rules are first-match-wins, so a dangerous
 * token anywhere in the command line must outrank the allows below,
 * and the guards (chaining, substitution, redirection, network) keep
 * every allow rule read-only even though its anchor only checks the
 * leading command.
 */
const READONLY_BASH_ARGS: ArgPolicy[] = [
  bashArg("deny", /\brm\s+(-[a-zA-Z]*r[a-zA-Z]*f|-[a-zA-Z]*f[a-zA-Z]*r|--recursive)/),
  bashArg("deny", /\b(mkfs\w*|dd\s+if=)/),
  bashArg("deny", /(>|>>).*\.env\b/),

  bashArg("ask", /\bsudo\b/),
  bashArg("ask", /(&&|\|\||;)/),
  bashArg("ask", /(\$\(|`)/),
  bashArg("ask", /(^|[|;&]\s*)(curl|wget|nc|ncat|socat|ssh|scp|sftp|rsync|telnet|ftp)\b/),
  bashArg("ask", /(>>?)\s*(?!(&|\/dev\/(null|tty)))\S/),

  bashArg("allow", /^(grep|egrep|fgrep)\b/),
  // Guards skip past pipes so only find's own arguments are checked.
  bashArg("allow", /^find\b(?![^;|&]*\s(-delete|-exec\w*|-ok\w*|-fprint\w*))/),
  bashArg("allow", /^(ls|ll|dir|tree|exa|eza)\b/),
  bashArg("allow", /^(cat|head|tail|less|more|bat|nl)\b/),
  bashArg("allow", /^(sort|uniq|cut|tr|column|tac|rev)\b/),
  bashArg("allow", /^(basename|dirname|realpath|readlink|which|whereis|type)\b/),
  bashArg("allow", /^(file|stat|wc|du|df|pwd)\b/),
  bashArg("allow", /^(whoami|id|uname|hostname|date|uptime)\b/),
  bashArg("allow", /^(diff|cmp|md5sum|sha1sum|sha256sum|shasum)\b/),
  bashArg("allow", /^jq\b/),
  bashArg("allow", /^sed\b(?![^;|&]*\s(-i|--in-place)\b)/),
  bashArg("allow", /^awk\b/),
  bashArg("allow", /^yq\b(?![^;|&]*\s(-i|--inplace)\b)/),
  bashArg("allow", /^git( -C \S+)? (status|diff|log|show|blame|rev-parse|describe|shortlog|reflog|ls-files|ls-remote|grep|stash list|worktree list|submodule status)\b/),
  // branch, tag, and remote mutate when given a name; allow only the
  // listing forms. A single-key "git config <key>" reads, two args set.
  bashArg("allow", /^git( -C \S+)? branch(?=$|\s+(-a|-r|-v|-vv|-l|--list|--show-current)\b)/),
  bashArg("allow", /^git( -C \S+)? tag(?=$|\s+(-l|-n\d*|--list)\b)/),
  bashArg("allow", /^git( -C \S+)? remote(?=$|\s+(-v|--verbose|show)\b)/),
  bashArg("allow", /^git( -C \S+)? config\s+(--get|--get-all|--get-regexp|--list|-l)\b/),
  bashArg("allow", /^git( -C \S+)? config\s+[\w.-]+$/),
  bashArg("allow", /^kubectl (get|describe|logs|top|version|api-resources|api-versions)\b/),
  bashArg("allow", /^(docker|podman) (ps|images|inspect|logs|version|stats)\b/),
];

/**
 * Modes used when the config file is missing or unusable. The tools
 * that ship with stock pi, classified:
 * - read: read, grep, find, ls, tool_search (no side effects).
 * - write: write, edit, layered over read.
 * - neither: bash and powershell run arbitrary commands, and codemode
 *   runs scripts that call other tools, so they stay at "ask"; bash
 *   gets arg-level allows for read-only commands only.
 */
const DEFAULT_MODE_DEFINITIONS: Record<string, ModeDefinition> = {
  read: {
    default: "ask",
    tools: {
      read: { policy: "allow" },
      grep: { policy: "allow" },
      find: { policy: "allow" },
      ls: { policy: "allow" },
      tool_search: { policy: "allow" },
      bash: { policy: "ask", args: READONLY_BASH_ARGS },
      powershell: { policy: "ask" },
      codemode: { policy: "ask" },
    },
  },
  write: {
    base: "read",
    tools: {
      write: { policy: "allow" },
      edit: { policy: "allow" },
    },
  },
};

function defaultModeConfig(): { mode: string; modes: ModesConfig } {
  return buildModesConfig(DEFAULT_MODE_DEFINITIONS, "read", []);
}

/** Used when the config file is missing or too broken to parse. */
export const DEFAULT_CONFIG: PermissionsConfig = { ...defaultModeConfig(), groups: EMPTY_GROUPS };

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

function parseToolPolicy(
  tool: string,
  value: unknown,
  errors: string[],
  prefix: string,
): ToolPolicy | undefined {
  const where = `${prefix}tools["${tool}"]`;
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

function parseToolPatterns(value: unknown, errors: string[], where: string): ToolPattern[] | undefined {
  if (!Array.isArray(value)) {
    errors.push(`${where}: must be an array of strings`);
    return undefined;
  }
  const parsed: ToolPattern[] = [];
  value.forEach((entry, i) => {
    if (typeof entry !== "string" || entry === "") {
      errors.push(`${where}[${i}]: must be a non-empty string`);
    } else {
      parsed.push({ pattern: entry, regex: toolPatternRegex(entry) });
    }
  });
  return parsed;
}

function parseGroupDefinition(name: string, value: unknown, errors: string[]): GroupDefinition | undefined {
  const where = `permissions.json: groups["${name}"]`;
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    errors.push(`${where}: must be an object`);
    return undefined;
  }
  const entry = value as Record<string, unknown>;

  const definition: GroupDefinition = { tools: [], read: [], write: [], default: "ask" };

  if (entry.description !== undefined) {
    if (typeof entry.description !== "string") {
      errors.push(`${where}: "description" must be a string`);
    } else {
      definition.description = entry.description;
    }
  }

  if (entry.tools === undefined) {
    errors.push(`${where}: "tools" is required`);
    return undefined;
  }
  const tools = parseToolPatterns(entry.tools, errors, `${where}.tools`);
  if (tools === undefined) {
    return undefined;
  }
  if (tools.length === 0) {
    errors.push(`${where}.tools: must list at least one tool pattern`);
    return undefined;
  }
  definition.tools = tools;

  if (entry.read !== undefined) {
    const read = parseToolPatterns(entry.read, errors, `${where}.read`);
    if (read) {
      definition.read = read;
    }
  }

  if (entry.write !== undefined) {
    const write = parseToolPatterns(entry.write, errors, `${where}.write`);
    if (write) {
      definition.write = write;
    }
  }

  if (entry.default !== undefined) {
    if (!isGroupLevel(entry.default)) {
      errors.push(`${where}: "default" must be one of ${GROUP_LEVELS.join(", ")}`);
    } else {
      definition.default = entry.default;
    }
  }
  return definition;
}

function parseGroups(value: unknown, errors: string[]): GroupsConfig {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    errors.push('permissions.json: "groups" must be an object keyed by group name');
    return EMPTY_GROUPS;
  }
  const groups: GroupsConfig = { order: [], definitions: {} };
  for (const [name, groupValue] of Object.entries(value)) {
    if (name === "") {
      errors.push("permissions.json: group names must be non-empty");
      continue;
    }
    const definition = parseGroupDefinition(name, groupValue, errors);
    if (definition) {
      groups.order.push(name);
      groups.definitions[name] = definition;
    }
  }
  return groups;
}

function parseModeDefinition(name: string, value: unknown, errors: string[]): ModeDefinition | undefined {
  const where = `permissions.json: modes["${name}"]`;
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    errors.push(`${where}: must be an object`);
    return undefined;
  }
  const entry = value as Record<string, unknown>;
  const definition: ModeDefinition = {};

  if (entry.base !== undefined) {
    if (typeof entry.base !== "string" || entry.base === "") {
      errors.push(`${where}: "base" must be a non-empty string`);
    } else {
      definition.base = entry.base;
    }
  }
  if (entry.hidden !== undefined) {
    if (typeof entry.hidden !== "boolean") {
      errors.push(`${where}: "hidden" must be a boolean`);
    } else {
      definition.hidden = entry.hidden;
    }
  }
  if (entry.default !== undefined) {
    const parsed = parseDecision(entry.default, errors, `${where}: "default"`);
    if (parsed) {
      definition.default = parsed;
    }
  }
  if (entry.tools !== undefined) {
    if (typeof entry.tools !== "object" || entry.tools === null || Array.isArray(entry.tools)) {
      errors.push(`${where}: "tools" must be an object keyed by tool name`);
    } else {
      const tools: Record<string, ToolPolicy> = {};
      for (const [tool, toolValue] of Object.entries(entry.tools)) {
        const parsed = parseToolPolicy(tool, toolValue, errors, `${where}.`);
        if (parsed) {
          tools[tool] = parsed;
        }
      }
      definition.tools = tools;
    }
  }
  return definition;
}

/**
 * Resolve every mode against its base chain and pick the initial mode.
 * Modes that fail to resolve are dropped with an error; modes that
 * inherit from a dropped mode fail in turn (their base is unknown). If
 * nothing survives, the built-in modes apply.
 */
function buildModesConfig(
  definitions: Record<string, ModeDefinition>,
  mode: string | undefined,
  errors: string[],
): { mode: string; modes: ModesConfig } {
  const order = Object.keys(definitions);
  const resolved: Record<string, PolicyConfig> = {};
  for (const name of order) {
    try {
      resolved[name] = resolveMode(definitions, name);
    } catch (err) {
      errors.push(`permissions.json: ${err instanceof Error ? err.message : String(err)}`);
    }
  }

  const valid = order.filter((name) => resolved[name] !== undefined);
  if (valid.length === 0) {
    errors.push("permissions.json: no usable modes; using built-in modes");
    return defaultModeConfig();
  }

  // Hidden modes stay reachable by name; they just never appear in
  // the shift+tab rotation.
  const cycle = valid.filter((name) => definitions[name].hidden !== true);

  let start = valid[0];
  if (mode !== undefined) {
    if (resolved[mode] !== undefined) {
      start = mode;
    } else {
      errors.push(`permissions.json: "mode" is not a defined mode (${mode}); using "${start}"`);
    }
  }
  return { mode: start, modes: { order: valid, cycle, resolved } };
}

export function loadPermissionsConfig(): { config: PermissionsConfig; errors: string[] } {
  const file = permissionsConfigPath();

  let raw: string;
  try {
    raw = fs.readFileSync(file, "utf8");
  } catch {
    // Missing or unreadable file: fall back to the built-in modes.
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

  const groups = root.groups === undefined ? EMPTY_GROUPS : parseGroups(root.groups, errors);

  if (typeof root.modes !== "object" || root.modes === null || Array.isArray(root.modes)) {
    errors.push('permissions.json: "modes" must be an object keyed by mode name');
    return { config: DEFAULT_CONFIG, errors };
  }
  if (root.default !== undefined || root.tools !== undefined) {
    errors.push('permissions.json: top-level "default"/"tools" do nothing; set them inside a mode');
  }

  let mode: string | undefined;
  if (root.mode !== undefined) {
    if (typeof root.mode !== "string" || root.mode === "") {
      errors.push('permissions.json: "mode" must be a non-empty string naming a mode');
    } else {
      mode = root.mode;
    }
  }

  const definitions: Record<string, ModeDefinition> = {};
  for (const [name, value] of Object.entries(root.modes)) {
    if (name === "") {
      errors.push("permissions.json: mode names must be non-empty");
      continue;
    }
    const definition = parseModeDefinition(name, value, errors);
    if (definition) {
      definitions[name] = definition;
    }
  }
  const built = buildModesConfig(definitions, mode, errors);
  return { config: { ...built, groups }, errors };
}
