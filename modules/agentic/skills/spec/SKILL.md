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
   MUST survive edits unless the user explicitly approves their change.
8. **Detect conflicts** (update flow only) — see [references/update-flow.md](references/update-flow.md).
   Any new input contradicting an existing MUST/SHOULD MUST be surfaced.
9. **Compose without writing** — produce the proposed spec in memory.
10. **Surface diff and conflicts** — present a section-level diff and every detected conflict.
    Offer resolution options for each conflict.
    Never auto-resolve a contradiction.
11. **Wait for explicit approval** — do not write to disk until the user approves.
12. **Write** — replace the file atomically.

## Flow-Specific Rules

### Bootstrap (no existing spec)

- Output a complete spec from scratch.
- Section selection is driven entirely by the inputs.
- A bootstrap diff is a diff against an empty document; show the full proposed content as the preview.
- Approval gate still applies before writing.

### Update (existing spec present)

- Read the existing spec in full first.
- Identify which sections the new inputs touch.
- Patch in place; do not regenerate untouched sections.
- Status line update: bump only if the user approves a substantive change.
  Cosmetic edits do not bump status.
- Every contradiction with an existing MUST/SHOULD is a conflict (see update-flow.md).

## Hard Gates

The following MUST hold for every invocation:

- **No write without approval.** Bootstrap and update flows both require explicit user approval before the file is written.
- **No silent supersession.** A new input contradicting an existing requirement MUST be surfaced as a conflict. The skill MUST present the user with resolution options. The skill MUST NOT pick a side.
- **No structural element rewrite as a side effect.** Numbered invariants, named error categories, pseudocode blocks, and inline defaults change only when the user explicitly approves the change.
- **No invented sections.** Only emit section types listed in [assets/spec-template.md](assets/spec-template.md). If a needed section type is missing, propose adding it to the template before using it.

## Output Contract

The produced spec MUST satisfy all of:

- Begins with a title and a status line (e.g. `Status: Draft v1`).
- Defines "Implementation-defined" once at the top, as a term used to mark intentionally open policy choices.
- Uses RFC 2119 keywords (MUST, MUST NOT, SHOULD, SHOULD NOT, MAY, REQUIRED, OPTIONAL, RECOMMENDED) for every normative statement.
- Has explicit Goals and Non-Goals sections.
- References external protocols and standards by name; does not restate them.
- Contains no changelog, revision history, or version table.

The full set of voice and structural rules lives in the references.

## Validation Checklist

Before requesting approval, verify:

- [ ] Status line present.
- [ ] "Implementation-defined" defined once at top.
- [ ] RFC 2119 keywords used for every normative statement; not as English synonyms.
- [ ] Goals and Non-Goals both present.
- [ ] Every section emitted is listed in the spec template.
- [ ] No section omitted because of laziness; only because its when-to-include criteria do not apply.
- [ ] (Update) Every structural element from the previous spec is present in the new spec, identically identified, unless the user approved a change.
- [ ] (Update) Section-level diff is ready to surface.
- [ ] (Update) Every conflict between new input and existing MUST/SHOULD is surfaced with options.
- [ ] No changelog, no version table, no in-document history.

## Anti-Patterns

- Producing a README- or design-doc-flavored document with "must" used as English. Use RFC 2119 keywords as graded normativity.
- Padding the spec with section types the inputs do not warrant (a stateless library has no state machine).
- Regenerating the whole spec on update; only patch sections the inputs touch.
- Silently picking a side when a new input contradicts an existing requirement.
- Bumping the status line on cosmetic edits.
- Embedding a changelog or version history in the spec body.
- Restating an external protocol the spec only needs to reference.

## When Not to Use

- The user has a single architectural decision to record — use `adr-writing` instead.
- The user is describing one product feature — use `product-definition` instead.
- The user wants to verify code conformance to an existing spec — that is a future skill, not this one.
- The user has not decided yet — run `brainstorm` first.

## Integration with Other Skills

- `brainstorm`, `product-definition`, `adr-writing` produce inputs this skill consumes.
- `writing-clearly-and-concisely` applies to spec prose.
- `commit` handles the commit after the user approves the write.

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
