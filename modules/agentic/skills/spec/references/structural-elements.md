# Structural Elements

Constructs in the spec that carry identity and MUST survive edits.
The skill MUST recognize each category, preserve it across updates, and surface any change explicitly in the diff presented to the user.

## Recognized Categories

### Numbered Invariants

Recognition pattern: a line of the form `Invariant N: ...` or `### Invariant N` followed by content.

Preservation rules:

- MUST NOT renumber across edits. Invariant 2 stays Invariant 2.
- MUST NOT rename. Naming is part of the invariant's identity.
- MAY append new invariants at the end of the list.
- A user-approved change to an invariant MUST appear explicitly in the diff.

### Named Error Categories

Recognition pattern: backtick-quoted identifiers in a list under a section titled "Error categories", "Failure classes", "Error mapping", or similar.

Examples: `missing_workflow_file`, `template_render_error`, `linear_api_status`.

Preservation rules:

- MUST NOT rename. Callers branch on the name.
- MAY add new categories at the end of the list.
- MAY remove a category only with explicit user approval.

### Pseudocode Blocks

Recognition pattern: fenced code blocks (` ```text `, ` ```pseudocode `, or ` ``` ` with no language) inside a "Reference Algorithms" or similarly named section.

Preservation rules:

- Preserve unless the referenced prose section is removed.
- MUST NOT rewrite the algorithm body during an unrelated edit.
- A user-approved change to the algorithm MUST appear explicitly in the diff.

### Inline Defaults

Recognition pattern: a default value stated within a configuration key's bullet or table row.

Example: `polling.interval_ms (integer) — default: 30000`.

Preservation rules:

- Defaults MUST stay inline with the field they belong to.
- A separate "Defaults" table MUST NOT be created.
- Changing a default value is a substantive change and MUST appear in the diff with the old and new values.

## Surfacing Changes

When a structural element is added, removed, or modified, the diff presented under the update flow MUST include:

- The category (invariant, error category, pseudocode block, inline default).
- The identifier (name or number).
- The old text and the new text, both shown.
- The user-resolved conflict that justifies the change, if applicable.

## Forbidden

- Renumbering an invariant as a side effect of inserting another one earlier in the list. If a new invariant is added, append it to the end.
- Renaming an error category to fit a new naming scheme. The old name stays; new categories follow whatever scheme the spec already uses.
- Moving a pseudocode block to a "cleaner" location during an unrelated edit.
- Extracting inline defaults into a separate table for "readability."
