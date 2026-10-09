/**
 * agent-rules extension
 *
 * Appends every .md file in <agent-dir>/rules/ to the system prompt, in a
 * dedicated "agent_rules" section. This lets you add instructions that only
 * apply in certain contexts without editing AGENTS.md.
 *
 * <agent-dir> is $PI_CODING_AGENT_DIR, or ~/.pi/agent when that is unset.
 *
 * Files are included verbatim in alphabetical filename order, each wrapped in
 * an <agent_rule path="..."> XML tag, in the same style pi uses to render
 * AGENTS.md (<project_instructions path="...">). Only top-level *.md files
 * are considered.
 *
 * Install: symlink (or copy) this file into <agent-dir>/extensions/.
 */

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

function agentRulesDir(): string {
  let agentDir = process.env.PI_CODING_AGENT_DIR ?? "";
  if (agentDir === "~" || agentDir.startsWith("~/")) {
    agentDir = path.join(os.homedir(), agentDir.slice(2));
  }
  if (!agentDir) {
    agentDir = path.join(os.homedir(), ".pi", "agent");
  }
  return path.join(agentDir, "rules");
}

function loadRulesSection(): string {
  const dir = agentRulesDir();

  let entries: string[];
  try {
    entries = fs.readdirSync(dir);
  } catch {
    return "";
  }

  const files = entries.filter((name) => name.endsWith(".md")).sort();
  const parts: string[] = [];
  for (const name of files) {
    const file = path.join(dir, name);
    const content = fs.readFileSync(file, "utf8").trim();
    if (content) {
      parts.push(`<agent_rule path="${file}">\n${content}\n</agent_rule>`);
    }
  }
  if (parts.length === 0) {
    return "";
  }
  return `Additional agent rules loaded from ${dir}/:\n\n${parts.join("\n\n")}`;
}

export default function agentRulesExtension(pi: ExtensionAPI) {
  pi.on("before_agent_start", (event) => {
    const content = loadRulesSection();
    if (!content) {
      return;
    }
    const sections = event.systemPromptOptions.sections;
    sections["agent_rules"] = sections["agent_rules"]
      ? `${sections["agent_rules"]}\n\n${content}`
      : content;
  });
}
