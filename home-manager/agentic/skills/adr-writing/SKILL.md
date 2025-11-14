---
name: adr-writing
description: "Use when documenting significant architectural decisions. Creates focused ADRs explaining context, decision, and alternatives. Prevents vague documentation and implementation detail bloat. Triggers: 'create ADR', 'document decision', making technology/framework/persistence/auth choices, cross-cutting concerns."
---

# ADR Writing

Architecture Decision Records document significant architectural choices.

Core principle: Explain why you decided, not how you'll implement.

## When to Create ADRs

Required for significant decisions (expensive or painful to reverse):

- Technology, framework, or language choices
- Data persistence strategies
- Communication patterns (sync/async, protocols)
- Authentication or authorization approaches
- Deployment strategies
- Cross-cutting concerns
- Decisions affecting system as a whole or multiple components

Not required for:

- Implementation details within a component
- Routine feature additions following established patterns
- Tactical coding decisions

## Required Sections

### Context

Document status quo and circumstances leading to decision:

- What exists today?
- What problem are we solving?
- What requirements (functional or non-functional) drive this?
- What constraints exist (team, budget, timeline, existing systems)?

Without status quo documentation, future readers cannot understand why change was needed.

### Decision

State what you're deciding and why:

- What are we deciding?
- Why is this the right choice given context and requirements?
- What specific requirements does this satisfy?

Focus on justification, not description.

### Options Considered

Document alternatives evaluated (2-3 main options):

- What alternatives were evaluated?
- What are specific trade-offs of each?
- For rejected options, provide concrete reasons

Concrete reasons for rejection:

- "Requires team retraining in technology X"
- "Adds monitoring complexity without benefits at our scale"
- "Incompatible with existing auth system"
- "Performance testing showed 3x latency increase"

Avoid vague reasons:

- "Has trade-offs" (everything has trade-offs)
- "Not ideal" (why not?)
- "Complexity" (what specific complexity?)

## Scope and Detail Level

ADRs document decisions, not implementations.

Include:

- High-level approach and rationale
- Key constraints and requirements satisfied
- Technology choices and why
- Component boundaries and responsibilities
- Error handling strategy at system level

Exclude:

- API schemas (unless decision is about API design pattern)
- Detailed configuration
- Step-by-step implementation
- Code examples (unless illustrating approach)

## File Naming

Name files by need, not solution.

Good:

- `handling-user-sessions.md`
- `storing-time-series-data.md`
- `authenticating-api-requests.md`

Bad:

- `redis-implementation.md` (names solution, not need)
- `jwt-tokens.md` (names solution, not need)

## Anti-Patterns

Forbidden without justification:

- Creating ADRs for routine decisions
- Documenting implementation details
- Vague trade-off analysis ("has pros and cons")
- Missing status quo documentation
- No concrete reasons for rejected options
- Prescribing exact implementation steps

## Integration with Other Skills

- writing-clearly-and-concisely: Apply to ADR prose
- tracing-knowledge-lineages: Review existing ADRs before creating new ones

## When Not to Use ADRs

Skip ADRs when:

- Decision follows established pattern
- Easy to reverse (low-risk change)
- Purely tactical (implementation detail)
- Team already aligned and context is obvious

Document these decisions in code comments or design docs instead.
