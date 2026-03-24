# Writing Guidelines

Reference material for skill body content, structure, and formatting.
Load this during the Green phase when writing or modifying skill content.

## Frontmatter Requirements

Required fields:

```yaml
---
name: skill-name
description: "[What it does]. Use when [applicable situations, triggers]. [Key capabilities]."
---
```

Optional fields:

```yaml
---
name: skill-name
description: "[What it does]. Use when [applicable situations, triggers]. [Key capabilities]."
license: Apache-2.0
compatibility: Requires git, docker, jq, and access to the internet
metadata:
  author: example-org
  version: "1.0"
  mcp-server: server-name
  category: workflow-automation
  tags: [project-management, automation]
allowed-tools: Bash(git:*) Read
---
```

Field constraints:

| Field           | Required | Constraints                                                                                                                                                                                                                   |
| --------------- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `name`          | Yes      | Max 64 chars. Unicode lowercase alphanumeric and hyphens only. Must not start/end with hyphen or contain consecutive hyphens. Must match parent directory name. Must not contain "claude" or "anthropic" (reserved prefixes). |
| `description`   | Yes      | Max 1024 chars. Single-line quoted string. Describes what skill does and when to use it.                                                                                                                                      |
| `license`       | No       | License name or reference to bundled license file.                                                                                                                                                                            |
| `compatibility` | No       | Max 500 chars. Environment requirements (product, packages, network).                                                                                                                                                         |
| `metadata`      | No       | Arbitrary key-value mapping for additional metadata.                                                                                                                                                                          |
| `allowed-tools` | No       | Space-delimited list of pre-approved tools. Experimental.                                                                                                                                                                     |

Frontmatter appears in the system prompt.
Never use XML angle brackets (`<` `>`) in any frontmatter field.

### Description Guidance

Use single-line quoted strings for descriptions.
Write descriptions in third person.
Include keywords matching error messages and symptoms.
Start with what the skill does, then "Use when..." for discoverability.
Include specific tasks users might say (e.g., "create a skill", "write tests").
Mention relevant file types if applicable (e.g., "SKILL.md", ".test.ts").
Use negative triggers to prevent over-triggering
(e.g., "Do NOT use for [unrelated task]").
See [search-optimization.md](search-optimization.md) for details.

## Structure Guidelines

SKILL.md must not exceed 500 lines.
Body content should stay under 5000 tokens.

Use progressive disclosure with three tiers:

1. **Metadata** (~100 tokens): `name` and `description` loaded at startup for all skills
2. **Instructions** (<5000 tokens): Full SKILL.md body loaded when skill activates
3. **Resources** (as needed): Files in `scripts/`, `references/`, `assets/` loaded on demand

Keep core guidance in SKILL.md.
Move detailed reference material to `references/` subdirectory.

### File References

Use relative paths from skill root for internal references:

```markdown
See [the reference guide](references/REFERENCE.md) for details.

Run the extraction script:
scripts/extract.py
```

Keep file references one level deep from SKILL.md.
Avoid deeply nested reference chains.

For other skills:
Use "skill-name skill" or "Follow skill-name" syntax.
Example: `Follow commit skill for commits`

## Body Structure

Include these sections in skill bodies:

- **Instructions** - step-by-step workflow with clear phases
- **Common Issues** - error handling and troubleshooting
- **Examples** - concrete scenarios with expected outputs

See [body-template.md](../assets/body-template.md) for a recommended template.

## Content Guidelines

Degrees of freedom must match task fragility:

- High-fragility tasks require low-freedom guidance (checklists, imperatives)
- Multi-approach problems require high-freedom guidance (principles, examples)

Examples:

- Source control, security, testing → checklists, "You must", "Never", "Required"
- Architecture, design patterns → principles, "Consider", moderate authority
- Research, exploration → context, goals, minimal constraints

Assume the agent is smart.
Never include redundant explanations.
Only explain what baseline knowledge lacks.

Skills generally fall into three categories:
Document and Asset Creation, Workflow Automation, and MCP Enhancement.
Each category has distinct techniques.
See [skill-categories.md](skill-categories.md) for details.

## Writing Anti-Pattern Sections

Anti-pattern sections teach agents what to avoid.
But labeling a command as "Bad" causes agents to avoid the command entirely,
even when a valid variant exists.
This is called label poisoning.

### The Problem

When a command appears under a "Bad" label:

- Agents treat the command name itself as forbidden
- A nearby "Good" variant using the same command is ignored
- Agents use complex workarounds instead of the correct variant

### The Rule

If a command has both dangerous and safe modes,
show the safe mode in a positive section first.
Only list the truly dangerous invocation as an anti-pattern.
Never label the command name itself as "Bad".

### Before (label-poisoned)

```markdown
## Anti-Patterns

**Using deploy (interactive, waits for input):**

# Bad: opens confirmation dialog
deploy staging

# Good: non-interactive
deploy --yes staging
```

Agents reading this avoid `deploy` entirely.

### After (correct)

```markdown
## Deployment

Always use non-interactive mode:
deploy --yes staging

## Anti-Patterns

Never:
- Run deploy without --yes in scripts (opens confirmation dialog, blocks automation)
```

Agents reading this use `deploy --yes` confidently.

## Persuasion Principles

Follow prompt-engineering skill for the full persuasion principles framework.

Apply principles matching skill type:

Discipline-enforcing skills: Authority + Commitment + Social Proof
Guidance skills: Moderate authority + Unity
Collaborative skills: Unity + Commitment

## Approach: Problem-First vs Tool-First

Choose the framing that fits the use case:

- **Problem-first**: User describes outcomes, skill orchestrates the tools.
  "I need to set up a project workspace" → skill handles tool selection and sequencing.
- **Tool-first**: User already has tools, skill teaches optimal workflows.
  "I have Notion MCP connected" → skill provides best practices and domain expertise.

## Workflow Patterns

Complex tasks require structured workflows agents can track.

Five common patterns:

1. **Sequential orchestration** —
   Multi-step processes in specific order with dependencies between steps.
   Validation at each stage, rollback instructions for failures.
2. **Multi-service coordination** —
   Workflows spanning multiple MCP servers.
   Clear phase separation, data passing between services,
   validation before moving to next phase.
3. **Iterative refinement** —
   Output quality improves through generate-validate-improve loops.
   Explicit quality criteria, validation scripts, defined stop conditions.
4. **Context-aware tool selection** —
   Same outcome, different tools depending on context.
   Clear decision criteria, fallback options, transparency about choices.
5. **Domain-specific intelligence** —
   Specialized knowledge beyond tool access.
   Compliance checks before action, comprehensive documentation.

For critical validations, prefer bundled scripts over language instructions.
Code is deterministic; language interpretation is not.

Validation-critical operations must use plan-validate-execute:

1. Generate plan
2. Validate plan before execution
3. Execute with verification

Batch operations must include validation loops before proceeding.

Destructive operations must require explicit user confirmation.
