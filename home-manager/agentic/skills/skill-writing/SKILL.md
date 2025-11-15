---
name: skill-writing
description: "Required when creating SKILL.md files for Claude Code agents. Specializes prompt-engineering TDD with agent-specific requirements: frontmatter format, persuasion principles, 500-line limit, progressive disclosure, quality gates. Triggers: 'create a skill', 'write a skill', 'new SKILL.md', 'create agent documentation'."
---

# Skill Writing

Skills are reference guides for proven techniques.

This skill specializes prompt-engineering for Claude configurations.
Apply TDD methodology from @prompt-engineering.md:
observe failures without the skill,
write minimal guidance addressing those failures,
then refactor to close loopholes.

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

Use single-line quoted strings for descriptions:

```yaml
---
name: skill-name
description: "Use when [trigger]. Does [purpose]. Triggers: [symptoms]."
---
```

Write descriptions in third person.
Include keywords matching error messages and symptoms.
Start with "Use when..." for discoverability.

### Structure Guidelines

SKILL.md must not exceed 500 lines.
Use progressive disclosure:
keep core guidance in SKILL.md,
link supporting details in separate files.

### Cross-Referencing

Distinguish between internal and external references:

Internal files (within same skill directory):
Use @file.md syntax for progressive disclosure.
Example: `See @examples.md for detailed patterns`

Other skills:
Use "skill-name skill" or "Follow skill-name" syntax.
Never use @ syntax for other skills.
Example: `Follow source-control-hygiene skill for commits`

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

See @persuasion-principles.md for detailed framework and ethical guidelines.

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

## Quality Gates

You cannot finalize until all gates pass:

1. Observed baseline failures without skill
2. Description uses third person and includes triggers
3. Frontmatter uses single-line quoted string
4. Core content under 500 lines
5. Tested across target models (sonnet minimum)
6. Keywords match likely search terms
7. Persuasion principles match skill type
8. Validation loops for destructive operations
9. No ALL CAPS (except acronyms), no emojis
10. Title matches frontmatter name

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

See @anti-patterns.md for complete list of forbidden practices.

## Search Optimization

See @search-optimization.md for keyword strategy and discoverability requirements.
