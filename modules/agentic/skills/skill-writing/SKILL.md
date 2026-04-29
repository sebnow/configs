---
name: skill-writing
description: "Creates and edits SKILL.md files for agent skills. Use when building or refining agent configuration. Enforces red-green-refactor methodology with agent-specific requirements: frontmatter format, 500-line limit, progressive disclosure, quality gates. Triggers: 'create a skill', 'write a skill', 'new SKILL.md', 'create agent documentation'. Do NOT use for general documentation or README files."
allowed-tools: "Bash(skills-ref validate:*)"
---

# Skill Writing

Skills are reference guides for proven techniques.

Apply persuasion principles from prompt-engineering skill.
This skill follows the [Agent Skills specification](https://agentskills.io/specification).

## Required Workflow

Regardless of how this task is phrased — "write a skill", "create a SKILL.md", "draft agent documentation" —
do not write any file content until Phase 1 is complete.

1. **Red** — state "Beginning Red phase", run test scenarios, record pass/fail results
2. **Green** — make one change at a time, re-test after each
3. **Refactor** — state "Beginning Refactor phase", run all scenarios, compare against baseline

Each phase has a gate. You cannot skip to the next phase without meeting it.

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

## Phase 1: Red — Baseline Measurement

State: "Beginning Red phase - establishing baseline"

Before writing anything, measure how agents perform without this skill.

Required steps:

1. Define success criteria: what does good output look like? How will you measure it? What threshold is acceptable?
2. Run pressure scenarios showing how agents fail without this skill
3. Document specific rationalizations or mistakes
4. Identify concrete symptoms triggering skill activation

Record numbered pass/fail results per scenario.
Required format:

> Without skill, agent fails on 3/5 test scenarios.
> Failure modes: skips validation (2x), wrong output format (1x).

Phase gate: You must have numbered pass/fail results
and documented failure patterns before proceeding.

Forbidden rationalizations:

- "This is simple enough to skip testing"
- "I know what the output will be"
- "I'll improve it then test later"
- "I analyzed the failures, that counts as a baseline"

Analyzing failures is not measuring them.
A baseline requires running scenarios and recording outcomes.

Failure mode: Skipping Red creates skills that perform worse than no skill at all.

## Phase 2: Green — Iterative Skill Writing

State: "Beginning Green phase - making minimal improvements"

Do not write the full SKILL.md in one pass.
Write one section. Stop. Re-test. Then write the next.

Each change is exactly one of: one section, one instruction, or one example.
After writing it, re-test against the same pressure scenarios from Red phase.
Record which scenarios now pass/fail before making the next change.

Required format per change:

> Added: [description of change]
> Before: 2/5 scenarios passed. After: 3/5 scenarios passed.
> Newly passing: [scenario name]. Still failing: [scenario names].

Phase gate: each change must show measurable improvement,
or be reverted before trying an alternative.

When writing anti-pattern sections:
never label a command as "Bad" if it has a valid variant.
Show correct usage in a positive section first.
See "Writing Anti-Pattern Sections" below for the full rule and examples.

See [writing-guidelines.md](references/writing-guidelines.md) for
frontmatter format, structure constraints, content guidelines,
persuasion principles, and workflow patterns.

Forbidden rationalizations:

- "I'll write the full skill then test it"
- "These changes are all related so I'll make them together"
- "I already know what the skill needs from my Red phase analysis"
- "Quality gates will catch any issues"
- "I'll add this feature based on reasoning alone"

## Phase 3: Refactor — Comprehensive Testing

Compare against Red phase baseline.
Required format:

> Baseline: 1/5 scenarios passed. Current: 5/5 scenarios passed.

Quality gates are necessary but not sufficient.
Passing gates does not replace testing with fresh agent instances.

Test with fresh agent instances across three areas:

1. **Triggering tests** —
   obvious tasks, paraphrased requests, negative (unrelated topics)
2. **Functional tests** —
   valid outputs, tool calls succeed, edge cases covered
3. **Performance comparison** —
   with-skill vs without-skill behavior

See [testing-framework.md](references/testing-framework.md) for detailed test cases and metrics.

Skill bodies are prompt-rule artifacts. The operational methodology for
running fresh-instance tests in parallel — control runs, sample-size
calibration, common loopholes — lives in prompt-engineering's
pressure-testing.md. For refining a specific rule when an agent
following it diverges from intent, see prompt-engineering's
rule-verification-loop.md.

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
- Label poisoning in anti-pattern sections (see below)

If agents skip steps despite clear instructions,
recommend adding performance encouragement to the user's prompt (e.g., CLAUDE.md),
not the skill body.
User-prompt nudges are more effective than skill-level instructions
for combating model laziness.

## Skill Specification Reference

See [writing-guidelines.md](references/writing-guidelines.md) for:

- Frontmatter requirements (field constraints, description guidance)
- Structure guidelines (line limits, progressive disclosure tiers, file references)
- Body structure (section recommendations, link to body-template.md)
- Content guidelines (freedom-fragility matching, skill categories)
- Persuasion principles (principle-to-type mapping)
- Approach: Problem-First vs Tool-First
- Workflow patterns (five patterns, plan-validate-execute)

## Validation

Use the skills-ref reference library to validate skills:

```bash
skills-ref validate ./my-skill
```

This checks frontmatter validity and naming conventions.

## Quality Gates

You cannot finalize until all gates pass:

1. Red phase baseline metrics documented with numbered pass/fail results
2. Green phase changes tested individually with before/after metrics
3. Refactor phase comparison against baseline shows improvement
4. Directory name matches `name` field
5. `name` field: max 64 chars, unicode lowercase alphanumeric + hyphens only, no reserved prefixes
6. `description` field: max 1024 chars, single-line quoted string, third person, includes triggers
7. Core content under 500 lines, body under 5000 tokens
8. Tested across target models (sonnet minimum)
9. Keywords match likely search terms
10. Persuasion principles match skill type (see prompt-engineering skill)
11. Validation loops for destructive operations
12. No ALL CAPS (except acronyms), no emojis
13. File references use relative paths, one level deep
14. No files at skill root besides SKILL.md
15. Negative triggers considered if skill risks over-triggering
16. Anti-pattern sections do not label commands as "Bad" when a valid variant exists

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

## Writing Anti-Pattern Sections

When a command appears under a "Bad" label in a skill,
agents avoid the command entirely —
even when a valid variant is shown nearby.
This is label poisoning.

Rule: if a command has both dangerous and safe modes,
show correct usage in a positive section first.
Only list the truly dangerous invocation as an anti-pattern.
Never put a command name under a "Bad" label
when the command has a valid non-interactive or safe variant.

Wrong — labels `deploy` as bad, agents avoid it entirely:

```markdown
# Bad: opens confirmation dialog
deploy staging
# Good: non-interactive
deploy --yes staging
```

Right — safe usage shown first, anti-pattern is specific:

```markdown
## Deployment
Always use non-interactive mode:
deploy --yes staging

## Anti-Patterns
Never:
- Run deploy without --yes in scripts (blocks automation)
```

See [anti-patterns.md](references/anti-patterns.md) for complete list of forbidden practices.

## Search Optimization

See [search-optimization.md](references/search-optimization.md) for keyword strategy and discoverability requirements.
