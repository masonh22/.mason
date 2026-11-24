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
 * Modes: shift+tab (or "/permissions") cycles between non-hidden
 * modes; "/permissions <name>" switches to any mode, including hidden
 * ones. The active mode is shown in the extension status row below the
 * footer and recorded in the session, so resuming restores it. Note
 * that pi reserves shift+tab for app.thinking.cycle unless
 * keybindings.json rebinds that action.
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

import { Box, Text } from "@earendil-works/pi-tui";
import type { ExtensionAPI, ExtensionContext, ToolCallEvent } from "@earendil-works/pi-coding-agent";
import { DEFAULT_CONFIG, loadPermissionsConfig } from "./config.ts";
import {
  describeInput,
  evaluate,
  type PermissionsConfig,
  type PolicyDecision,
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

const AUDIT_ENTRY_TYPE = "permissions";
const MODE_ENTRY_TYPE = "permissions-mode";
/** Footer status key; the status row shows the active mode. */
const STATUS_KEY = "permissions";

export default function permissionsExtension(pi: ExtensionAPI) {
  let config: PermissionsConfig = DEFAULT_CONFIG;
  /** Active mode name. */
  let activeMode: string = config.mode;

  pi.on("session_start", async (_event, ctx) => {
    const loaded = loadPermissionsConfig();
    config = loaded.config;
    for (const error of loaded.errors) {
      ctx.ui.notify(`permissions: ${error}`, "warning");
    }
    activeMode = config.mode;
    const restored = restoreMode(ctx);
    if (restored !== undefined) {
      activeMode = restored;
    }
    updateStatus(ctx);
  });

  /** Show the active mode in the extension status row below the footer. */
  const updateStatus = (ctx: ExtensionContext): void => {
    ctx.ui.setStatus(STATUS_KEY, `mode: ${activeMode}`);
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

  const setMode = (name: string, ctx: ExtensionContext): void => {
    activeMode = name;
    pi.appendEntry(MODE_ENTRY_TYPE, {
      mode: name,
      timestamp: new Date().toISOString(),
    } satisfies PermissionModeEntry);
    updateStatus(ctx);
    ctx.ui.notify(`permissions mode: ${name}`, "info");
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
    description: "Cycle permissions mode, or switch to a named mode",
    handler: async (args, ctx) => {
      const name = args.trim();
      if (name === "") {
        cycleMode(ctx);
        return;
      }
      // config.modes.order includes hidden modes; they are
      // explicit-switch only.
      const order = config.modes.order;
      if (!order.includes(name)) {
        ctx.ui.notify(`permissions: unknown mode "${name}" (available: ${order.join(", ")})`, "warning");
        return;
      }
      setMode(name, ctx);
    },
  });

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
    const policy = evaluate(event.toolName, event.input, config.modes.resolved[activeMode]);

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
