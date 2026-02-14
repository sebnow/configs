---
name: skill-writing
description: "Use when creating or editing SKILL.md files for agent skills. Enforces prompt-engineering TDD with agent-specific requirements: frontmatter format, persuasion principles, 500-line limit, progressive disclosure, quality gates. Triggers: 'create a skill', 'write a skill', 'new SKILL.md', 'create agent documentation'."
---

# Skill Writing

Skills are reference guides for proven techniques.

This skill specializes prompt-engineering for Claude configurations.
Apply TDD methodology from prompt-engineering skill:
observe failures without the skill,
write minimal guidance addressing those failures,
then refactor to close loopholes.

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

You must observe baseline behavior before creating documentation.

State: "Beginning Red phase - observing baseline behavior"

Required steps:

1. Run pressure scenarios showing how agents fail without this skill
2. Document specific rationalizations or mistakes
3. Identify concrete symptoms triggering skill activation

You cannot proceed to Green phase until you complete all steps.

Forbidden rationalizations:
- "This is simple enough to skip testing"
- "I know what the agent will do"
- "I'll write documentation then test later"
- "Quick documentation is better than none"

If you did not watch an agent fail without the skill,
you do not know if the skill teaches the right thing.

Failure mode:
Skipping Red creates ineffective skills that waste tokens without changing behavior.

## Writing: The Green Phase

State: "Beginning Green phase - writing minimal documentation"

Create minimal documentation addressing observed failures.

### Frontmatter Requirements

Required fields:

```yaml
---
name: skill-name
description: "Use when [trigger]. Does [purpose]. Triggers: [symptoms]."
---
```

Optional fields:

```yaml
---
name: skill-name
description: "Use when [trigger]. Does [purpose]. Triggers: [symptoms]."
license: Apache-2.0
compatibility: Requires git, docker, jq, and access to the internet
metadata:
  author: example-org
  version: "1.0"
allowed-tools: Bash(git:*) Read
---
```

Field constraints:

| Field           | Required | Constraints                                                              |
| --------------- | -------- | ------------------------------------------------------------------------ |
| `name`          | Yes      | Max 64 chars. Unicode lowercase alphanumeric and hyphens only. Must not start/end with hyphen or contain consecutive hyphens. Must match parent directory name. |
| `description`   | Yes      | Max 1024 chars. Single-line quoted string. Describes what skill does and when to use it. |
| `license`       | No       | License name or reference to bundled license file.                       |
| `compatibility` | No       | Max 500 chars. Environment requirements (product, packages, network).    |
| `metadata`      | No       | Arbitrary key-value mapping for additional metadata.                     |
| `allowed-tools` | No       | Space-delimited list of pre-approved tools. Experimental.                |

Use single-line quoted strings for descriptions.
Write descriptions in third person.
Include keywords matching error messages and symptoms.
Start with "Use when..." for discoverability.

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

### Persuasion Principles

Every skill must apply persuasion principles matching its type.

Discipline-enforcing skills: Authority + Commitment + Social Proof
Guidance skills: Moderate authority + Unity
Collaborative skills: Unity + Commitment

See [persuasion-principles.md](references/persuasion-principles.md) for detailed framework and ethical guidelines.

### Workflow Patterns

Complex tasks require checklist-style workflows agents can track.

Validation-critical operations must use plan-validate-execute:

1. Generate plan
2. Validate plan before execution
3. Execute with verification

Batch operations must include validation loops before proceeding.

Destructive operations must require explicit user confirmation.

## Refining: The Refactor Phase

State: "Beginning Refactor phase - testing with fresh instances"

Test with fresh agent instances.
Observe where they struggle.
Close loopholes discovered during testing.

Required testing:

- Run skill with target models (sonnet, haiku, opus)
- Verify discoverability through keyword search
- Confirm token efficiency for frequently-loaded skills
- Test progressive disclosure (do links load correctly?)

Common loopholes:

- Agent skips required validation steps
- Agent rationalizes avoiding tests
- Agent misses edge cases in instructions
- Description lacks activation keywords

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
3. `name` field: max 64 chars, unicode lowercase alphanumeric + hyphens only
4. `description` field: max 1024 chars, single-line quoted string, third person, includes triggers
5. Core content under 500 lines, body under 5000 tokens
6. Tested across target models (sonnet minimum)
7. Keywords match likely search terms
8. Persuasion principles match skill type
9. Validation loops for destructive operations
10. No ALL CAPS (except acronyms), no emojis
11. File references use relative paths, one level deep
12. No files at skill root besides SKILL.md

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
