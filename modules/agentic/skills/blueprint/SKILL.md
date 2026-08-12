---
name: blueprint
description: "Condenses the decided context (brainstorm output, a ticket, prior conversation, codebase exploration) into a single structured design document for one change: the modules it touches, the call flow, and the decisions behind it. Use when the change is decided and needs a structured plan before implementation. Triggers: 'write a blueprint', 'design this change', 'plan this change', 'blueprint'. Do NOT interview — run the brainstorm skill first if the change is undecided. Do NOT use for permanent decision records (use adr-writing) or whole-system specs (use spec)."
disable-model-invocation: true
---

# Blueprint

Condense the decided context into a single structured design document for one change.

It condenses whatever context the agent currently holds
(brainstorm output, a ticket, prior conversation, codebase exploration)
into a structure an implementing agent can act on.
It covers one change: an incremental change, or the first slice of a greenfield.

## What this is not

- Not a product spec — no user stories, no product metrics.
- Not an ADR. An ADR records a single decision; this document plans a whole change and may cover several decisions.
- Not a spec. A spec is the whole-system present truth; this plans one change.

## Input: decided context

This skill consolidates; it does not calibrate.
Calibration happens beforehand — typically via the `brainstorm` skill,
or an existing ticket or design note.

Typical inputs:

- A brainstorm transcript
- A ticket or external design note
- Prior conversation and codebase exploration in this session

Do not interview the user.
If the context is too thin to fill a section,
record the gap in Open Questions (see the Open Questions gate below).

## Workflow

1. **Gather context** — synthesize what is already decided. Do not re-interview.
2. **Sketch the design** — identify the modules the change touches and map the call flow.
3. **Write the document** — fill the structure using the template.
4. **Get approval** — present the document; never begin implementation before approval.

## Design: modules and call flow

The Design section holds both the module interfaces and the call flow.
They cross-check each other:
every call named in the tree corresponds to a module's interface signature.

### Modules

A deep module has a narrow public interface and substantial internal complexity.
For each module the change introduces or modifies, capture:

- **Name** — what the module is called
- **Responsibility** — one sentence
- **Interface signature** — the public surface as a code block in the language that fits the module's area
  (a signature pins down parameter names, types, and return shape exactly;
  a prose description leaves those for the next agent to infer)
- **Contracts** — invariants, error modes, side effects

Specify interfaces, not internals. Downstream agents own the implementation.
This includes database schema, event, or other API signatures.

The signature is the declaration only — no `{ ... }` body, no example data.
Do not include implementation bodies, schemas with column lists, configuration values, file paths, or line numbers.
Those belong in the issues produced downstream.

### Call flow

Map how control flows through the change as an indented tree.
The root is the entry point (what triggers the flow);
children are the calls it makes, in order.
Prefix every line with a marker column, unified-diff style: a space for unchanged
calls, `+` for added calls, `-` for removed calls, with the tree indentation after
the marker — so a ```diff block highlights it.

Every call named in the tree must correspond to a module interface signature above.
The tree shows the sequence; the signatures pin the contracts.
See [assets/blueprint-template.md](assets/blueprint-template.md) for a worked example.

For a change that alters no control flow (pure data or schema change),
write "No control-flow change" and let the Modules subsection stand alone.

## Document structure

Use [assets/blueprint-template.md](assets/blueprint-template.md) as the structure.
Sections in order:

1. **Context**
2. **Design** (modules + call flow)
3. **Decisions**
4. **Testing**
5. **Out of Scope**
6. **Open Questions**

## Language

Write for an engineer who did not join the design discussion.
The reader can write code but does not know this change.
Give the reader enough to do the work without more questions.

- Simple, declarative sentences
- Plain English (avoid: "utilize", "leverage", "facilitate")
- Explicit over implicit

Avoid:

- Vague terms ("user-friendly", "intuitive", "seamless")
- Passive voice ("shall be provided", "is enabled")
- Implementation bodies, configuration values, or full schema definitions
  (interface signatures in the Design section are required — those stay)

## Common Mistakes

**Over-specification**
Do not include implementation bodies, full schemas with column lists,
configuration values, file paths, or line numbers.
Those belong in the issues produced downstream.

**Under-specification**
Each module must have a signature and contracts.
Each decision must be testable, unambiguous, and necessary.

## Validation Checklist

Before saving the document, verify:

- [ ] Input was decided context (brainstorm, ticket, or prior conversation) — no inline interview
- [ ] Each module entry has an interface signature in a fitting language, plus contracts
- [ ] The call flow tree is present (or "No control-flow change" is noted), and every call maps to a module signature
- [ ] Decisions are recorded with their rationale
- [ ] Testing identifies which modules get tests
- [ ] Out of Scope contains at least one non-trivial exclusion
- [ ] Open Questions is empty; if not, calibration was requested (see the gate below)
- [ ] No implementation bodies, schemas, configuration values, file paths, or line numbers anywhere
- [ ] User asked for approval before issue breakdown begins

## Open Questions gate

Open Questions should be empty.
Calibration happens before this document, so unresolved questions mean the context was too thin.

If any remain, do not proceed to implementation. State:

> "The design document has unresolved open questions: [list].
> These should be resolved before implementation.
> Run the brainstorm skill to calibrate, then return."

## After the document

State: "Design document complete. Please review and approve before issue breakdown."

Wait for explicit approval.
Do not proceed with any coding, architecture, or design work until the user confirms.

## Downstream consumer

The document feeds the `breakdown` skill,
which slices it into vertical issues an executing agent can act on
without re-exploring the codebase.

To support this:

- Design must let an agent identify the affected module(s) per issue
  and copy each signature forward verbatim
- Testing must let an agent know which behavior to verify
- Out of Scope must let an agent reject creep without re-asking

If a fresh agent cannot draft 3 vertical-slice issues from the document alone,
the document is incomplete — return to the Design step and tighten module interfaces.

## Common Issues

### Context is too thin to fill a section

Cause: the change was not calibrated before the document was requested.
Solution: do not start an inline interview. Record each gap in Open Questions and apply the Open Questions gate.

### Module sketch lacks signatures or contracts

Cause: the context captured decisions but not the signature or error modes of each module.
Solution: infer the most likely signature per module from the context, picking the language that fits the module's area. Flag each inferred signature in Open Questions so the team can validate before implementation.

### User wants to skip the approval step

Cause: the user is eager to start implementation immediately.
Solution: remind the user the document may contain incorrect assumptions. Skipping review risks wasted implementation work. Do not proceed without explicit approval.

### The document contains implementation detail

Cause: the context captured file paths, implementation bodies, full schemas, or configuration values.
Solution: reduce each to a signature plus a prose contract in the Design section. Keep the declaration; remove the body, the schema columns, the config values.

## Examples

### Example: brainstorm transcript → design document

Input: the user shares a brainstorm summary covering a problem restatement, three decisions, and two open questions.
Actions: (1) Synthesize the decided context. (2) Sketch the design: name each module, state its responsibility, write its public signature in a fitting language, state its contracts, then map the call flow as a diff-marked tree. (3) Write the document in section order (Context → Design → Decisions → Testing → Out of Scope → Open Questions). (4) Present it and ask for approval.
Result: a complete design document whose call flow cross-checks against the module signatures, with the two original open questions carried into the gate.

### Example: change is undecided

Input: the user says "write a blueprint for our new notification system" with no prior calibration.
Actions: do not start an inline interview. Explain that the change must be calibrated first. Direct the user to run the brainstorm skill and return with its output.
Result: no document is started. The user is unblocked toward the correct predecessor step.
