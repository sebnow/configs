---
name: skill-writing
description: "Creates and edits SKILL.md files for agent skills. Use when building or refining agent configuration. Enforces red-green-refactor methodology with agent-specific requirements: frontmatter format, 500-line limit, progressive disclosure, quality gates. Triggers: 'create a skill', 'write a skill', 'new SKILL.md', 'create agent documentation'. Do NOT use for general documentation or README files."
allowed-tools: "Bash(skills-ref validate:*)"
---

# Skill Writing

Skills are reference guides for proven techniques.

This skill specializes red-green-refactor methodology for Claude configurations.
Follow red-green-refactor skill for the core TDD methodology.
Apply persuasion principles from prompt-engineering skill.

This skill follows the [Agent Skills specification](https://agentskills.io/specification).

## Directory Structure

A skill is a directory containing at minimum a `SKILL.md` file:

```
skill-name/
├── SKILL.md          # Required - main skill instructions
├── scripts/          # Optional - executable code agents can run
├── references/       # Optional - additional documentation loaded on demand
└── assets/           # Optional - static resources (templates, schemas, images)
```

The directory name must match the `name` field in frontmatter.
`SKILL.md` is the only file allowed at the skill root.
All other files must be in `scripts/`, `references/`, or `assets/`.

## Before Writing: The Red Phase

Follow red-green-refactor skill for core Red phase methodology.

Required domain-specific steps:

1. Run pressure scenarios showing how agents fail without this skill
2. Document specific rationalizations or mistakes
3. Identify concrete symptoms triggering skill activation

## Writing the Skill

Create minimal documentation addressing observed failures.

### Frontmatter Requirements

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

Use single-line quoted strings for descriptions.
Write descriptions in third person.
Include keywords matching error messages and symptoms.
Start with what the skill does, then "Use when..." for discoverability.
Include specific tasks users might say (e.g., "create a skill", "write tests").
Mention relevant file types if applicable (e.g., "SKILL.md", ".test.ts").
Use negative triggers to prevent over-triggering
(e.g., "Do NOT use for [unrelated task]").
See [search-optimization.md](references/search-optimization.md) for details.

### Structure Guidelines

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
Example: `Follow source-control-hygiene skill for commits`

### Body Structure

Include these sections in skill bodies:

- **Instructions** - step-by-step workflow with clear phases
- **Common Issues** - error handling and troubleshooting
- **Examples** - concrete scenarios with expected outputs

See [body-template.md](assets/body-template.md) for a recommended template.

### Content Guidelines

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
See [skill-categories.md](references/skill-categories.md) for details.

### Persuasion Principles

Follow prompt-engineering skill for the full persuasion principles framework.

Apply principles matching skill type:

Discipline-enforcing skills: Authority + Commitment + Social Proof
Guidance skills: Moderate authority + Unity
Collaborative skills: Unity + Commitment

### Approach: Problem-First vs Tool-First

Choose the framing that fits the use case:

- **Problem-first**: User describes outcomes, skill orchestrates the tools.
  "I need to set up a project workspace" → skill handles tool selection and sequencing.
- **Tool-first**: User already has tools, skill teaches optimal workflows.
  "I have Notion MCP connected" → skill provides best practices and domain expertise.

### Workflow Patterns

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

## Refining: The Refactor Phase

Follow red-green-refactor skill for core Refactor phase methodology.

Test with fresh agent instances across three areas:

1. **Triggering tests** -
   obvious tasks, paraphrased requests, negative (unrelated topics)
2. **Functional tests** -
   valid outputs, tool calls succeed, edge cases covered
3. **Performance comparison** -
   with-skill vs without-skill behavior

See [testing-framework.md](references/testing-framework.md) for detailed test cases and metrics.

Additionally:

- Run skill with target models (sonnet minimum)
- Verify discoverability through keyword search
- Confirm token efficiency for frequently-loaded skills
- Test progressive disclosure (do links load correctly?)

Common loopholes:

- Agent skips required validation steps
- Agent rationalizes avoiding tests
- Agent misses edge cases in instructions
- Description lacks activation keywords
- Instructions too verbose (causes non-compliance; use bullet points)
- Critical instructions buried below less important content
- Ambiguous language ("make sure" instead of "Before calling X, verify Y")

If agents skip steps despite clear instructions,
recommend adding performance encouragement to the user's prompt (e.g., CLAUDE.md),
not the skill body.
User-prompt nudges are more effective than skill-level instructions
for combating model laziness.

## Validation

Use the skills-ref reference library to validate skills:

```bash
skills-ref validate ./my-skill
```

This checks frontmatter validity and naming conventions.

## Quality Gates

You cannot finalize until all gates pass:

1. Observed baseline failures without skill
2. Directory name matches `name` field
3. `name` field: max 64 chars, unicode lowercase alphanumeric + hyphens only, no reserved prefixes
4. `description` field: max 1024 chars, single-line quoted string, third person, includes triggers
5. Core content under 500 lines, body under 5000 tokens
6. Tested across target models (sonnet minimum)
7. Keywords match likely search terms
8. Persuasion principles match skill type (see prompt-engineering skill)
9. Validation loops for destructive operations
10. No ALL CAPS (except acronyms), no emojis
11. File references use relative paths, one level deep
12. No files at skill root besides SKILL.md
13. Negative triggers considered if skill risks over-triggering

State: "All quality gates passed" before finalizing.

## Token Efficiency

Context windows are shared resources.

Required techniques:

- Cross-reference existing skills instead of repeating content
- Progressive disclosure keeps frequently-loaded content minimal
- Assume baseline knowledge
- Remove all redundant explanations
- Use semantic line breaks for readability without bloat

Frequently-loaded skills must target under 200 words total.

## Anti-Patterns

See [anti-patterns.md](references/anti-patterns.md) for complete list of forbidden practices.

## Search Optimization

See [search-optimization.md](references/search-optimization.md) for keyword strategy and discoverability requirements.
