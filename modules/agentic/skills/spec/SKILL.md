---
name: spec
description: "Produces and maintains a holistic system specification (SPEC.md) at a project root. The spec is the single source of truth for what the system must conform to. Use when starting a new project's spec, folding new ADRs or requirements into an existing spec, or reconciling contradicting decisions. Triggers: 'create SPEC.md', 'write a spec', 'system specification', 'update the spec', 'fold ADR into spec', 'specify the system'. Do NOT use for PRDs (run product-definition) or ADRs (run adr-writing); the spec is downstream of both."
---

# Spec Writing

Produces a single, authoritative `SPEC.md` describing what the system must conform to right now.

The spec is a snapshot of present truth.
It is not a changelog, not a PRD, not a design doc.
History lives in git.
Decision rationale lives in ADRs.
The spec answers only "what must hold."

## Predecessors and Inputs

This skill consumes decided inputs.
It does not interview the user.

Accepted input sources:

- Brainstorm transcripts (output of the `brainstorm` skill)
- PRDs (output of the `product-definition` skill)
- ADRs (output of the `adr-writing` skill)
- Loose requirement notes or design decisions

If the input is incomplete or contradictory,
flag the gap and continue with what is decided.
Do not start an inline interview.

## Workflow

You MUST follow this sequence.
Skipping a step is a defect, not a shortcut.

1. **Resolve output location** — default `SPEC.md` at repo root.
   Accept an override path if the user supplies one.
2. **Detect flow** — file exists at output location → update flow.
   Absent → bootstrap flow.
3. **Read existing spec in full** (update flow only) — before composing any changes.
4. **Ingest inputs** — read every supplied input source.
5. **Select sections** — start from [assets/spec-template.md](assets/spec-template.md) as the canonical skeleton and conditional-section menu.
   Pick from the menu based on input shape.
   Do not pad with sections the inputs do not warrant.
   Strip the template's guidance comments before writing.
6. **Apply voice rules** — see [references/voice-guide.md](references/voice-guide.md).
   RFC 2119 normativity, status line, "Implementation-defined" definition,
   external protocols referenced not redefined.
7. **Preserve structural elements** (update flow only) —
   see [references/structural-elements.md](references/structural-elements.md).
   Numbered invariants, named error categories, pseudocode blocks, inline defaults
   MUST survive edits unless the user has explicitly directed their change.
8. **Detect and resolve conflicts** (update flow only) — see [references/update-flow.md](references/update-flow.md).
   Any new input contradicting an existing requirement MUST be surfaced
   with resolution options. Wait for the user's decision on each contradiction.
   Never auto-resolve.
9. **Write** — apply the changes to the file directly.
   The file is tracked in source control; the user iterates via normal edits
   and VCS, not via an in-chat preview of the full spec.
10. **Report** — list the sections added or modified in one or two sentences.
    Surface any open questions or gaps in the inputs.

## Flow-Specific Rules

### Bootstrap (no existing spec)

- Output a complete spec from scratch.
- Section selection is driven entirely by the inputs.
- Write the file directly. The user iterates by editing the file or by re-invoking the skill with additional inputs.

### Update (existing spec present)

- Read the existing spec in full first.
- Identify which sections the new inputs touch.
- Patch in place; do not regenerate untouched sections.
- Status line update: bump only on a substantive change.
  Cosmetic edits do not bump status.
- Every contradiction with an existing requirement is a conflict (see update-flow.md)
  and pauses for user resolution before the write.
  Severity-specific resolution options live in update-flow.md.

## Hard Gates

The following MUST hold for every invocation:

- **No silent supersession.** A new input contradicting an existing requirement MUST be surfaced as a conflict before the write. The skill MUST present the user with resolution options. The skill MUST NOT pick a side.
- **No structural element rewrite as a side effect.** Numbered invariants, named error categories, pseudocode blocks, and inline defaults change only when the user has explicitly directed the change.
- **No invented sections.** Only emit section types listed in [assets/spec-template.md](assets/spec-template.md). If a needed section type is missing, propose adding it to the template before using it.
- **No in-chat preview of the full spec before writing.** The file is tracked; the chat is for conflict resolution and post-write reporting, not for previewing content the user can read on disk.

## Output Contract

The produced spec MUST satisfy all of:

- Begins with a title and a status line (e.g. `Status: Draft v1`).
- Defines "Implementation-defined" once at the top, as a term used to mark intentionally open policy choices.
- Uses RFC 2119 keywords (MUST, MUST NOT, SHOULD, SHOULD NOT, MAY, REQUIRED, OPTIONAL, RECOMMENDED) for every normative statement.
- Has explicit Goals and Non-Goals sections.
- References external protocols and standards by name; does not restate them.
- Contains no changelog, revision history, or version table.
- Describes the system at the contract level, not the implementation level.
  Internal packages, modules, language-specific types, DDL, and infrastructure
  technologies MUST NOT appear in normative prose unless they are themselves
  the public contract. See [references/voice-guide.md](references/voice-guide.md)
  under "Level of Description".

The full set of voice and structural rules lives in the references.

## Validation Checklist

Before writing, verify:

- [ ] Status line present.
- [ ] "Implementation-defined" defined once at top.
- [ ] RFC 2119 keywords used for every normative statement; not as English synonyms.
- [ ] Goals and Non-Goals both present.
- [ ] No internal package, module, file, or directory path used as a canonical identifier in normative prose.
- [ ] No language-specific types in field signatures; domain model uses language-neutral types only.
- [ ] No DDL, table names, columns, or grants in normative prose unless the database schema is the public contract.
- [ ] No internal infrastructure technologies named in requirements about internal coupling.
- [ ] Test matrix and Implementation Checklist describe observable behaviors, not code-path or module deliverables.
- [ ] Each requirement states an externally observable invariant, not the mechanism (cache ordering, upsert flag, index/constraint shape, transaction boundary) the implementation uses to satisfy it.
- [ ] Non-functional solutions (caching, indexing, batching, queueing) appear only where the externally observable property they serve (latency, atomicity scope, ordering guarantee, throughput target) is also specified.
- [ ] When the system has more than one component, a System Overview / Components section precedes the Core Domain Model and names components by role.
- [ ] Every section emitted is listed in the spec template.
- [ ] No section omitted because of laziness; only because its when-to-include criteria do not apply.
- [ ] (Update) Every structural element from the previous spec is present in the new spec, identically identified, unless the user directed a change.
- [ ] (Update) Every conflict between new input and an existing requirement has been surfaced and resolved before the write.
- [ ] No changelog, no version table, no in-document history.

## Anti-Patterns

- Producing a README- or design-doc-flavored document with "must" used as English. Use RFC 2119 keywords as graded normativity.
- Padding the spec with section types the inputs do not warrant (a stateless library has no state machine).
- Regenerating the whole spec on update; only patch sections the inputs touch.
- Silently picking a side when a new input contradicts an existing requirement.
- Bumping the status line on cosmetic edits.
- Embedding a changelog or version history in the spec body.
- Restating an external protocol the spec only needs to reference.
- Smuggling implementation choices into normative prose: internal package paths, language-specific types, DDL for internal storage, named internal infrastructure. The spec describes the contract; implementation belongs in code and design docs.
- Specifying mechanism (cache ordering, upsert with flag, index/constraint shape, transaction boundary) instead of the externally observable invariant the mechanism preserves. Apply the "replace with somehow" litmus test; if the requirement still has meaning, the mechanism was incidental.

## When Not to Use

- The user has a single architectural decision to record — use `adr-writing` instead.
- The user is describing one product feature — use `product-definition` instead.
- The user wants to verify code conformance to an existing spec — that is a future skill, not this one.
- The user has not decided yet — run `brainstorm` first.

## Integration with Other Skills

- `brainstorm`, `product-definition`, `adr-writing` produce inputs this skill consumes.
- `writing-clearly-and-concisely` applies to spec prose.
- `commit` handles the commit after the user has reviewed the written file.

## Common Issues

### Input has no normative structure

Cause: The user passed a brainstorm transcript or a bullet list.
Solution: That is expected. This skill imposes the structure; inputs do not need to.

### User asks for a multi-file spec

Cause: A section has grown large enough to feel like its own document.
Solution: Multi-file splitting is out of scope for v1. Keep the spec single-file. Note the size as an open question for a future iteration.

### Conflict with no clean resolution

Cause: A new input contradicts an existing requirement in a way that cannot be both-and-ed.
Solution: Surface the conflict with at least three options: keep existing, replace with new, keep both with user-supplied reconciliation. Do not pick.
