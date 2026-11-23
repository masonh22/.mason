/**
 * Approval prompt UI for the permissions extension.
 *
 * TUI mode renders a custom dialog:
 * - Number keys (1-9) select and submit the matching option directly.
 * - Select up/down keys move between options.
 * - Enter submits the highlighted option.
 * - Hovering "Deny" and typing a non-digit starts an inline reason;
 *   Enter denies with that reason (an empty reason falls back to the
 *   default message).
 * - Esc while typing goes back to the options.
 * - Esc at the options level denies with the default reason (fail closed).
 *
 * Non-TUI modes with a UI (e.g. RPC) fall back to a plain Allow/Deny
 * select dialog with no reason field.
 */

import type { ExtensionContext } from "@earendil-works/pi-coding-agent";
import {
  Editor,
  visibleWidth,
  wrapTextWithAnsi,
  type EditorTheme,
} from "@earendil-works/pi-tui";

export type Approval = { action: "allow" } | { action: "deny"; reason?: string };

/** One selectable option in the dialog. */
interface PermissionOption {
  label: string;
  action: "allow" | "deny";
}

/** Options shown in the dialog, in display order. */
const OPTIONS: PermissionOption[] = [
  { label: "Allow", action: "allow" },
  { label: "Deny", action: "deny" },
];

/** True when data is a single printable character (no control keys). */
function isPrintable(data: string): boolean {
  if (data.length !== 1) return false;
  const code = data.charCodeAt(0);
  return code >= 32 && code !== 127;
}

/** True when data is a single digit 1-9 (option shortcut keys). */
function isOptionDigit(data: string): boolean {
  return data.length === 1 && data >= "1" && data <= "9";
}

function wrappedWithPrefix(lines: string[], prefix: string, text: string, width: number): void {
  const prefixWidth = visibleWidth(prefix);
  if (prefixWidth >= width) {
    lines.push(...wrapTextWithAnsi(prefix + text, width));
    return;
  }
  const wrapped = wrapTextWithAnsi(text, width - prefixWidth);
  const continuationPrefix = " ".repeat(prefixWidth);
  for (let i = 0; i < wrapped.length; i++) {
    lines.push(`${i === 0 ? prefix : continuationPrefix}${wrapped[i]}`);
  }
}

/**
 * Ask the user to approve or deny a tool call.
 * toolName: name of the tool being called.
 * inputDescription: pre-formatted, multi-line summary of the tool input.
 */
export async function promptApproval(
  ctx: ExtensionContext,
  toolName: string,
  inputDescription: string,
): Promise<Approval> {
  if (ctx.mode !== "tui") {
    // Fallback for other UI modes: plain select dialog, no reason field.
    const choice = await ctx.ui.select(`Allow ${toolName}?\n\n${inputDescription}`, [
      "Allow",
      "Deny",
    ]);
    return choice === "Allow" ? { action: "allow" } : { action: "deny" };
  }

  const result = await ctx.ui.custom<Approval | null>((tui, theme, keybindings, done) => {
    const titleLines = [`Allow ${toolName}?`, ...inputDescription.split("\n")];
    let optionIndex = 0;
    let editMode = false;
    let cachedLines: string[] | undefined;

    const editorTheme: EditorTheme = {
      borderColor: (s) => theme.fg("accent", s),
      selectList: {
        selectedPrefix: (t) => theme.fg("accent", t),
        selectedText: (t) => theme.fg("accent", t),
        description: (t) => theme.fg("muted", t),
        scrollInfo: (t) => theme.fg("dim", t),
        noMatch: (t) => theme.fg("warning", t),
      },
    };
    const editor = new Editor(tui, editorTheme);

    editor.onSubmit = (value) => {
      const reason = value.trim();
      done({ action: "deny", reason: reason || undefined });
    };

    const refresh = () => {
      cachedLines = undefined;
      tui.requestRender();
    };

    function handleInput(data: string): void {
      if (editMode) {
        // While typing a reason, everything goes to the editor except the
        // select cancel key(s), which return to the options.
        if (keybindings.matches(data, "tui.select.cancel")) {
          editMode = false;
          editor.setText("");
          refresh();
          return;
        }
        editor.handleInput(data);
        refresh();
        return;
      }

      // Navigation and confirm/cancel follow the user's configured
      // tui.select.* keybindings (e.g. ctrl+p / ctrl+n), like built-in
      // select dialogs.
      if (keybindings.matches(data, "tui.select.up")) {
        optionIndex = Math.max(0, optionIndex - 1);
        refresh();
        return;
      }
      if (keybindings.matches(data, "tui.select.down")) {
        optionIndex = Math.min(OPTIONS.length - 1, optionIndex + 1);
        refresh();
        return;
      }

      if (keybindings.matches(data, "tui.select.confirm")) {
        submit(optionIndex);
        return;
      }

      if (keybindings.matches(data, "tui.select.cancel")) {
        // Fail closed: cancel means deny with the default reason.
        done(null);
        return;
      }

      // Number keys select and submit the matching option directly.
      if (isOptionDigit(data)) {
        const index = data.charCodeAt(0) - "1".charCodeAt(0);
        if (index < OPTIONS.length) {
          submit(index);
        }
        // Digits without a matching option are ignored.
        return;
      }

      // Start typing a reason directly while a deny option is highlighted.
      if (OPTIONS[optionIndex].action === "deny" && isPrintable(data)) {
        editMode = true;
        editor.handleInput(data);
        refresh();
      }
    }

    /** Submit an option by index. Denials via shortcut carry no reason. */
    function submit(index: number): void {
      const action = OPTIONS[index].action;
      if (action === "deny") {
        done({ action: "deny" });
      } else {
        done({ action: "allow" });
      }
    }

    function render(width: number): string[] {
      if (cachedLines) return cachedLines;

      const lines: string[] = [];
      const w = Math.max(1, width);

      lines.push(theme.fg("accent", "-".repeat(w)));
      for (let i = 0; i < titleLines.length; i++) {
        const line = titleLines[i];
        // The input preview after the first line is rendered muted.
        if (i === 0) {
          wrappedWithPrefix(lines, " ", theme.fg("text", line), w);
        } else {
          wrappedWithPrefix(lines, " ", theme.fg("muted", line), w);
        }
      }
      lines.push("");

      for (let i = 0; i < OPTIONS.length; i++) {
        const selected = i === optionIndex;
        const editing = OPTIONS[i].action === "deny" && editMode;
        const prefix = selected ? theme.fg("accent", "> ") : "  ";
        const label = `${i + 1}. ${OPTIONS[i].label}${editing ? " (reason below)" : ""}`;
        const color = selected || editing ? "accent" : "text";
        wrappedWithPrefix(lines, prefix, theme.fg(color, label), w);
      }

      if (editMode) {
        lines.push("");
        wrappedWithPrefix(lines, " ", theme.fg("muted", "Reason (optional):"), w);
        for (const line of editor.render(Math.max(1, w - 2))) {
          lines.push(` ${line}`);
        }
      }

      lines.push("");
      if (editMode) {
        wrappedWithPrefix(lines, " ", theme.fg("dim", "Enter to deny - Esc to go back"), w);
      } else {
        wrappedWithPrefix(
          lines,
          " ",
          theme.fg("dim", "1/2 or Enter select - type on Deny for a reason - Esc denies"),
          w,
        );
      }
      lines.push(theme.fg("accent", "-".repeat(w)));

      cachedLines = lines;
      return lines;
    }

    return {
      render,
      invalidate: () => {
        cachedLines = undefined;
      },
      handleInput,
    };
  });

  if (result === null) {
    return { action: "deny" };
  }
  return result;
}
