/**
 * permissions extension
 *
 * Gates every tool call the agent makes through a permission policy.
 *
 * Policy: rules are loaded from <agent-dir>/permissions.json (see
 * permissions.example.json for a documented example). The file defines
 * named modes; each call is evaluated against the active mode. Tools
 * not covered by any rule follow the mode's "default" (ask when unset).
 * Arg-level regex rules are consulted before tool-level and default
 * policies.
 *
 * Groups: the config can also define named tool groups (typically MCP
 * servers); see policy.ts for how a group's level gates its tools. A
 * tool claimed by a group never reaches the mode policy.
 *
 * Modes: shift+tab cycles between non-hidden modes; "/permissions <name>"
 * switches to any mode, including hidden ones. "/permissions groups"
 * lists groups, and "/permissions groups <name> <level>" sets one. In
 * TUI mode "/permissions" opens a menu for both; elsewhere it cycles
 * modes, so RPC keeps working. The active mode and group levels are
 * shown in the extension status row below the footer and recorded in
 * the session, so resuming restores them. Note that pi reserves
 * shift+tab for app.thinking.cycle unless keybindings.json rebinds
 * that action.
 *
 * Behavior:
 * - "allow": runs without prompting.
 * - "deny": blocks the call. Denying blocks only that call; the agent
 *   sees the denial reason and can continue. It does not terminate the
 *   run.
 * - "ask" (default): shows an approve/deny prompt before execution.
 *   The user can type a denial reason; without one a default message
 *   is used.
 * - With no UI (headless mode), "ask" calls are denied (fail closed).
 *
 * The denial message sent to the model is self-describing (tool, mode,
 * rule, user reason), because pi's block API carries only that string.
 *
 * Additionally, every denial is recorded as a "permissions" custom
 * session entry (tool, mode, rule, source, reason, timestamp). Unlike
 * the denial message, these audit entries are not sent to the model;
 * they persist in the session file and render in the transcript only.
 * Approvals are not logged, to keep session noise down.
 */

import { Box, Container, SettingsList, Text, type SettingItem } from "@earendil-works/pi-tui";
import { getSettingsListTheme, type ExtensionAPI, type ExtensionContext, type ToolCallEvent } from "@earendil-works/pi-coding-agent";
import { DEFAULT_CONFIG, loadPermissionsConfig } from "./config.ts";
import {
  describeInput,
  evaluate,
  evaluateGroup,
  GROUP_LEVELS,
  isGroupLevel,
  type GroupLevel,
  type PermissionsConfig,
  type PolicyDecision,
  type ToolPattern,
} from "./policy.ts";
import { promptApproval } from "./prompt.ts";

/** How a denial was reached. */
type AuditSource = "user" | "policy" | "no-ui";

/** Data stored in each "permissions" custom session entry. */
interface PermissionDenial {
  toolName: string;
  toolCallId: string;
  /** Policy rule that produced the decision. */
  rule: string;
  source: AuditSource;
  /** Active mode when the call was denied. */
  mode: string;
  /** User-provided denial reason, when given. */
  reason?: string;
  timestamp: string;
}

/** Data stored in each "permissions-mode" custom session entry. */
interface PermissionModeEntry {
  mode: string;
  timestamp: string;
}

/** Data stored in each "permissions-group" custom session entry. */
interface PermissionGroupEntry {
  group: string;
  level: GroupLevel;
  timestamp: string;
}

const AUDIT_ENTRY_TYPE = "permissions";
const MODE_ENTRY_TYPE = "permissions-mode";
const GROUP_ENTRY_TYPE = "permissions-group";
/** Footer status key; the status row shows the active mode. */
const STATUS_KEY = "permissions";

export default function permissionsExtension(pi: ExtensionAPI) {
  let config: PermissionsConfig = DEFAULT_CONFIG;
  /** Active mode name. */
  let activeMode: string = config.mode;
  /** Current level per group name. */
  let groupLevels: Record<string, GroupLevel> = {};

  pi.on("session_start", async (_event, ctx) => {
    const loaded = loadPermissionsConfig();
    config = loaded.config;
    for (const error of loaded.errors) {
      ctx.ui.notify(`permissions: ${error}`, "warning");
    }
    activeMode = config.mode;
    groupLevels = {};
    for (const name of config.groups.order) {
      groupLevels[name] = config.groups.definitions[name].default;
    }
    const restored = restoreMode(ctx);
    if (restored !== undefined) {
      activeMode = restored;
    }
    restoreGroupLevels(ctx);
    updateStatus(ctx);
  });

  /** Show the active mode, plus groups off their default level, in the status row. */
  const updateStatus = (ctx: ExtensionContext): void => {
    let status = `mode: ${activeMode}`;
    const changed = config.groups.order.filter(
      (name) => groupLevels[name] !== config.groups.definitions[name].default,
    );
    if (changed.length > 0) {
      status += ` | ${changed.map((name) => `${name}: ${groupLevels[name]}`).join(", ")}`;
    }
    ctx.ui.setStatus(STATUS_KEY, status);
  };

  /** Last mode recorded in this session's active branch, if still valid. */
  const restoreMode = (ctx: ExtensionContext): string | undefined => {
    const entry = ctx.sessionManager
      .getBranch()
      .findLast((e) => e.type === "custom" && e.customType === MODE_ENTRY_TYPE);
    const mode = (entry?.data as PermissionModeEntry | undefined)?.mode;
    // The newest record names a mode that no longer exists; older
    // records are unlikely to fare better.
    return typeof mode === "string" && config.modes.resolved[mode] !== undefined
      ? mode
      : undefined;
  };

  /** Replay group-level records from the active branch; last write per group wins. */
  const restoreGroupLevels = (ctx: ExtensionContext): void => {
    for (const entry of ctx.sessionManager.getBranch()) {
      if (entry.type !== "custom" || entry.customType !== GROUP_ENTRY_TYPE) {
        continue;
      }
      const data = entry.data as PermissionGroupEntry | undefined;
      if (data && isGroupLevel(data.level) && Object.hasOwn(config.groups.definitions, data.group)) {
        groupLevels[data.group] = data.level;
      }
    }
  };

  const setMode = (name: string, ctx: ExtensionContext, notify = true): void => {
    activeMode = name;
    pi.appendEntry(MODE_ENTRY_TYPE, {
      mode: name,
      timestamp: new Date().toISOString(),
    } satisfies PermissionModeEntry);
    updateStatus(ctx);
    if (notify) {
      ctx.ui.notify(`permissions mode: ${name}`, "info");
    }
  };

  const setGroupLevel = (
    name: string,
    level: GroupLevel,
    ctx: ExtensionContext,
    notify = true,
  ): void => {
    groupLevels[name] = level;
    pi.appendEntry(GROUP_ENTRY_TYPE, {
      group: name,
      level,
      timestamp: new Date().toISOString(),
    } satisfies PermissionGroupEntry);
    updateStatus(ctx);
    if (notify) {
      ctx.ui.notify(`permissions group ${name}: ${level}`, "info");
    }
  };

  const cycleMode = (ctx: ExtensionContext): void => {
    const cycle = config.modes.cycle;
    if (cycle.length < 2) {
      ctx.ui.notify("permissions: no other mode to cycle to; use /permissions <name>", "info");
      return;
    }
    // A hidden active mode is not in the cycle; indexOf returns -1
    // and the rotation restarts from the first cyclable mode.
    const index = cycle.indexOf(activeMode);
    setMode(cycle[(index + 1) % cycle.length], ctx);
  };

  pi.registerShortcut("shift+tab", {
    description: "Cycle permissions mode",
    handler: (ctx) => cycleMode(ctx),
  });

  pi.registerCommand("permissions", {
    description: "Permissions menu; switch mode or set a group level",
    handler: async (args, ctx) => {
      const tokens = args.trim().split(/\s+/).filter(Boolean);

      if (tokens.length === 0) {
        // TUI gets the interactive menu; other modes cycle so RPC
        // mode switching stays scriptable.
        if (ctx.mode === "tui") {
          await showMenu(ctx);
        } else {
          cycleMode(ctx);
        }
        return;
      }

      // The literal "groups" always routes to the groups subcommand;
      // a mode with that name is shadowed.
      if (tokens[0] === "groups") {
        groupsCommand(tokens.slice(1), ctx);
        return;
      }

      if (tokens.length > 1) {
        ctx.ui.notify(
          "usage: /permissions [mode] | /permissions groups [name [level]]",
          "warning",
        );
        return;
      }

      // config.modes.order includes hidden modes; they are
      // explicit-switch only.
      const name = tokens[0];
      const order = config.modes.order;
      if (!order.includes(name)) {
        ctx.ui.notify(`permissions: unknown mode "${name}" (available: ${order.join(", ")})`, "warning");
        return;
      }
      setMode(name, ctx);
    },
  });

  /** "/permissions groups [name [level]]": list groups or set a level. */
  const groupsCommand = (tokens: string[], ctx: ExtensionContext): void => {
    const order = config.groups.order;

    if (tokens.length === 0) {
      if (order.length === 0) {
        ctx.ui.notify("permissions: no groups configured", "info");
        return;
      }
      const lines = order.map((name) => describeGroup(name));
      ctx.ui.notify(`permissions groups:\n${lines.join("\n")}`, "info");
      return;
    }

    const name = tokens[0];
    if (!order.includes(name)) {
      ctx.ui.notify(
        `permissions: unknown group "${name}" (available: ${order.join(", ")})`,
        "warning",
      );
      return;
    }

    if (tokens.length === 1) {
      const group = config.groups.definitions[name];
      const list = (patterns: ToolPattern[]) =>
        patterns.length > 0 ? patterns.map((p) => p.pattern).join(", ") : "(none)";
      ctx.ui.notify(
        `permissions group ${describeGroup(name)}\n` +
          `read tools: ${list(group.read)}\n` +
          `write tools: ${list(group.write)}`,
        "info",
      );
      return;
    }

    if (tokens.length > 2) {
      ctx.ui.notify(
        `usage: /permissions groups ${name} <${GROUP_LEVELS.join("|")}>`,
        "warning",
      );
      return;
    }

    const level = tokens[1];
    if (!isGroupLevel(level)) {
      ctx.ui.notify(
        `permissions: level must be one of ${GROUP_LEVELS.join(", ")}`,
        "warning",
      );
      return;
    }
    setGroupLevel(name, level, ctx);
  };

  const describeGroup = (name: string): string => {
    const group = config.groups.definitions[name];
    const level = groupLevels[name];
    const suffix = level === group.default ? "" : ` (default: ${group.default})`;
    return `${name}: ${level}${suffix} [${group.tools.map((t) => t.pattern).join(", ")}]`;
  };

  /** Interactive settings list for the mode and group levels (TUI only). */
  const showMenu = async (ctx: ExtensionContext): Promise<void> => {
    await ctx.ui.custom((tui, theme, _keybindings, done) => {
      const items: SettingItem[] = [
        {
          id: "mode",
          label: "mode",
          description: "Permissions mode",
          currentValue: activeMode,
          values: config.modes.order,
        },
        ...config.groups.order.map((name) => {
          const group = config.groups.definitions[name];
          return {
            id: `group:${name}`,
            label: name,
            description: group.description ?? group.tools.map((t) => t.pattern).join(", "),
            currentValue: groupLevels[name],
            values: [...GROUP_LEVELS],
          } satisfies SettingItem;
        }),
      ];

      const list = new SettingsList(
        items,
        Math.min(items.length + 2, 15),
        getSettingsListTheme(),
        (id, newValue) => {
          if (id === "mode") {
            setMode(newValue, ctx, false);
          } else {
            setGroupLevel(id.slice("group:".length), newValue as GroupLevel, ctx, false);
          }
        },
        () => done(undefined),
      );

      const container = new Container();
      container.addChild(new Text(theme.fg("accent", theme.bold("Permissions")), 0, 0));
      container.addChild(new Text("", 0, 0));
      container.addChild(list);

      return {
        // Let PageUp/PageDown page the list instead of scrolling the
        // fullscreen transcript.
        capturesViewportInput: list.capturesViewportInput,
        render(width: number) {
          return container.render(width);
        },
        invalidate() {
          container.invalidate();
        },
        handleInput(data: string) {
          list.handleInput?.(data);
          tui.requestRender();
        },
      };
    });
  };

  const auditDenial = (entry: PermissionDenial): void => {
    try {
      pi.appendEntry(AUDIT_ENTRY_TYPE, entry);
    } catch {
      // Auditing is best effort; never block a decision on logging.
    }
  };

  /** Record a denial and build the block result sent to the model. */
  const denyCall = (event: ToolCallEvent, policy: PolicyDecision, source: AuditSource, reason?: string) => {
    auditDenial({
      toolName: event.toolName,
      toolCallId: event.toolCallId,
      rule: policy.rule,
      source,
      mode: activeMode,
      ...(reason ? { reason } : {}),
      timestamp: new Date().toISOString(),
    });
    const detail = source === "no-ui" ? "no UI available to ask for approval" : (reason ?? "no reason given");
    return {
      block: true,
      reason: `[permissions] ${event.toolName} denied (mode: ${activeMode}, rule: ${policy.rule}): ${detail}`,
    };
  };

  // Compact transcript renderer for permission denials.
  pi.registerEntryRenderer(AUDIT_ENTRY_TYPE, (entry, { expanded }, theme) => {
    const d = entry.data as PermissionDenial;
    const source = d.source === "user" ? "" : ` (${d.source})`;
    const reason = d.reason ? ` - ${d.reason}` : "";
    const summary =
      theme.fg("error", `[deny] ${d.toolName}`) +
      theme.fg("dim", ` mode: ${d.mode} rule: ${d.rule}${source}${reason}`);

    if (!expanded) {
      return new Text(summary);
    }
    const box = new Box(1, 0);
    box.addChild(new Text(summary));
    box.addChild(new Text(theme.fg("dim", JSON.stringify(d, null, 2))));
    return box;
  });

  pi.on("tool_call", async (event, ctx) => {
    // Groups claim tools first; everything else falls to the mode.
    const policy =
      evaluateGroup(event.toolName, config.groups, groupLevels) ??
      evaluate(event.toolName, event.input, config.modes.resolved[activeMode]);

    if (policy.decision === "allow") {
      return undefined;
    }

    if (policy.decision === "deny") {
      return denyCall(event, policy, "policy");
    }

    // policy.decision === "ask": prompt the user.
    if (!ctx.hasUI) {
      // No UI available to ask; fail closed.
      return denyCall(event, policy, "no-ui");
    }

    const approval = await promptApproval(ctx, event.toolName, describeInput(event.input));

    if (approval.action === "allow") {
      return undefined;
    }

    return denyCall(event, policy, "user", approval.reason);
  });
}
