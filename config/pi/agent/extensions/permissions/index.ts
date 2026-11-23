/**
 * permissions extension
 *
 * Prompts the user to approve or deny every tool call the agent makes.
 * All agent interactions with the outside world go through tool calls:
 * built-in tools (bash, read, write, edit, find, grep, ls), custom tools
 * registered by extensions, and MCP tools. The "tool_call" event covers
 * all of them, so a single hook is enough for the basic version.
 *
 * Behavior:
 * - "ask" (default): shows an approve/deny prompt before execution. The
 *   user can type a denial reason; without one a default message is used.
 * - Denying blocks only that call; the agent sees the denial reason and
 *   can continue. It does not terminate the run.
 * - With no UI (headless mode), every call is denied (fail closed).
 *
 * The denial message sent to the model is self-describing (tool, rule,
 * user reason), because pi's block API carries only that string.
 *
 * Additionally, every denial is recorded as a "permissions" custom
 * session entry (tool, rule, source, reason, timestamp). Unlike the
 * denial message, these audit entries are not sent to the model; they
 * persist in the session file and render in the transcript only.
 * Approvals are not logged, to keep session noise down.
 */

import { Box, Text } from "@earendil-works/pi-tui";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { describeInput, evaluate } from "./policy.ts";
import { promptApproval } from "./prompt.ts";

/** How a denial was reached. */
type AuditSource = "user" | "policy" | "no-ui";

/** Data stored in each "permissions" custom session entry. */
interface PermissionDenial {
  toolName: string;
  toolCallId: string;
  decision: "deny";
  /** Policy rule that produced the decision. */
  rule: string;
  source: AuditSource;
  /** User-provided denial reason, when given. */
  reason?: string;
  timestamp: string;
}

const AUDIT_ENTRY_TYPE = "permissions";

export default function permissionsExtension(pi: ExtensionAPI) {
  const auditDenial = (entry: PermissionDenial): void => {
    try {
      pi.appendEntry(AUDIT_ENTRY_TYPE, entry);
    } catch {
      // Auditing is best effort; never block a decision on logging.
    }
  };

  // Compact transcript renderer for permission denials.
  pi.registerEntryRenderer(AUDIT_ENTRY_TYPE, (entry, { expanded }, theme) => {
    const d = entry.data as PermissionDenial;
    const source = d.source === "user" ? "" : ` (${d.source})`;
    const reason = d.reason ? ` - ${d.reason}` : "";
    const summary =
      theme.fg("error", `[deny] ${d.toolName}`) + theme.fg("dim", ` rule: ${d.rule}${source}${reason}`);

    if (!expanded) {
      return new Text(summary);
    }
    const box = new Box(1, 0);
    box.addChild(new Text(summary));
    box.addChild(new Text(theme.fg("dim", JSON.stringify(d, null, 2))));
    return box;
  });

  pi.on("tool_call", async (event, ctx) => {
    const policy = evaluate(event.toolName, event.input);
    const timestamp = new Date().toISOString();

    if (policy.decision === "allow") {
      return undefined;
    }

    if (policy.decision === "deny") {
      auditDenial({
        toolName: event.toolName,
        toolCallId: event.toolCallId,
        decision: "deny",
        rule: policy.rule,
        source: "policy",
        timestamp,
      });
      return {
        block: true,
        reason: `[permissions] ${event.toolName} denied (rule: ${policy.rule})`,
      };
    }

    // policy.decision === "ask": prompt the user.
    if (!ctx.hasUI) {
      // No UI available to ask; fail closed.
      auditDenial({
        toolName: event.toolName,
        toolCallId: event.toolCallId,
        decision: "deny",
        rule: policy.rule,
        source: "no-ui",
        timestamp,
      });
      return {
        block: true,
        reason: `[permissions] ${event.toolName} denied (rule: ${policy.rule}, no UI available to ask for approval)`,
      };
    }

    const approval = await promptApproval(ctx, event.toolName, describeInput(event.input));

    if (approval.action === "allow") {
      return undefined;
    }

    const denialReason = approval.reason
      ? `[permissions] ${event.toolName} denied (rule: ${policy.rule}): ${approval.reason}`
      : `[permissions] ${event.toolName} denied (rule: ${policy.rule}), no reason given`;

    auditDenial({
      toolName: event.toolName,
      toolCallId: event.toolCallId,
      decision: "deny",
      rule: policy.rule,
      source: "user",
      ...(approval.reason ? { reason: approval.reason } : {}),
      timestamp,
    });

    return { block: true, reason: denialReason };
  });
}
