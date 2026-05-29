# Update Flow

The process for folding new inputs into an existing spec.

The spec file is tracked in source control. The user iterates by editing the
file or re-invoking this skill. The chat is for conflict resolution and a
brief post-write report — not for previewing content the user can read on disk.

## Sequence

You MUST follow this sequence on every update invocation.

1. **Read the existing spec in full** before deciding any changes.
2. **Ingest each input source** in turn. Note the sections it touches.
3. **Detect conflicts** — see "Conflict Detection" below.
4. **Surface every conflict** with resolution options. Wait for the user's
   decision on each before writing.
5. **Apply the patch directly to the file.** Patch only sections the new inputs
   touch; other sections stay byte-identical.
6. **Report** the sections added or modified in one or two sentences.

## Conflict Detection

A conflict exists when a new input asserts a requirement that contradicts an existing requirement in the spec.

Contradictions to look for:

- New input says `MUST X`; existing spec says `MUST NOT X`.
- New input says `MUST X`; existing spec says `MUST Y` where X and Y are mutually exclusive.
- New input changes a default value the existing spec specifies inline.
- New input renames or removes a named error category the existing spec defines.
- New input changes the identity, number, or wording of an existing invariant.
- New input moves an existing goal into the non-goals list, or vice versa.

If detection is ambiguous (the input could be read as either consistent or contradicting), surface the ambiguity as a conflict, not as a silent decision.

## Conflict Surfacing

For each detected conflict, present:

```markdown
**Conflict in §<section> — <one-line summary>**

Existing (`SPEC.md` §<section>):
> <quoted existing requirement>

New input (<source identifier>):
> <quoted new requirement>

Resolution options:
1. Keep existing. Discard new input.
2. Replace with new. Existing requirement is removed.
3. Keep both. User supplies reconciling text.
4. <If existing is SHOULD/SHOULD NOT> Downgrade existing to MAY. Add new as MUST.

Which option, or other?
```

The skill MUST NOT proceed past a conflict without an explicit user resolution.

## Severity Rules

- **Conflict with an existing MUST or MUST NOT.** Flag and ask. Options 1, 2, 3 only. No downgrade option (MUST is not downgradeable without acknowledging the loss).
- **Conflict with an existing SHOULD or SHOULD NOT.** Flag and ask. Add option 4: downgrade existing to MAY.
- **Conflict with an existing MAY.** Flag and ask. Options 1, 2, 3. (User may upgrade MAY to MUST.)

The skill MUST NOT auto-resolve any conflict, regardless of severity.

## ADR Back-References

When an input is an ADR, the spec MAY incorporate a sparse back-reference at the point of the decision:

```markdown
The orchestrator does not persist scheduler state across restarts
(ADR-0042).
```

Back-references MUST be sparse: only at non-obvious or contested decision points. Do not back-reference at every requirement. The spec MUST remain readable without following any ADR link.

If an ADR is the source of a conflict resolution, the back-reference is appropriate. If an ADR confirms a decision the spec already encodes, a back-reference is noise.

## Status Line

Bump the status line only on a substantive change.
A substantive change is one that affects a MUST, MUST NOT, SHOULD, or SHOULD NOT requirement, an invariant, an error category, an inline default, or a goal.

Cosmetic changes (rewording without changing normative force, fixing typos) do not bump the status.

## Hard Gates

- **No silent conflict resolution.** Every detected contradiction reaches the user before the write.
- **No structural element rewrite as a side effect.** See [structural-elements.md](structural-elements.md).
- **No changelog in the produced spec.** History lives in git.

## Common Failure Modes

### Silently overwriting a MUST clause

Cause: A new input says "MUST always X" and the existing spec says "MUST stream X." The skill picks one without asking.
Fix: Always surface the contradiction. Even if the new input "seems more recent" or "is from the user," the resolution is the user's call.

### Bumping status on cosmetic edits

Cause: The skill bumps `Draft v1` to `Draft v2` after a typo fix.
Fix: Only bump on substantive changes. A typo fix is not a v2.

### Regenerating untouched sections

Cause: The skill rewrites the whole spec on update, eroding hand-edits.
Fix: Patch only sections the new inputs touch. Other sections stay byte-identical.

### Moving a goal into non-goals as a workaround

Cause: A new input contradicts a goal; the skill silently moves the goal to the non-goals list to make the contradiction disappear.
Fix: A goal-to-non-goal move is a substantive change. It MUST be surfaced as a conflict, not as an edit.
