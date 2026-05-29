# Spec Voice Guide

Rules and worked examples for the prose conventions every produced spec MUST follow.

## RFC 2119 Normative Vocabulary

Every normative statement MUST use one of these keywords:

- `MUST` / `MUST NOT` / `REQUIRED` / `SHALL` / `SHALL NOT` — absolute requirement; absolute prohibition.
- `SHOULD` / `SHOULD NOT` / `RECOMMENDED` — strong recommendation; deviation MUST be justified.
- `MAY` / `OPTIONAL` — truly discretionary.

The spec MUST cite RFC 2119 once near the top. The exact citation wording lives in [assets/spec-template.md](../assets/spec-template.md) under "Normative Language."

These words are graded normativity, not English emphasis. Reserve them for requirement statements.

### Before / After

A produced spec MUST read as the right column, not the left.

| Vague | Normative |
|---|---|
| "The tool should fail fast on bad input." | "The tool MUST exit non-zero on malformed input unless `--lenient` is set." |
| "It needs to support streaming." | "The tool MUST stream JSONL and TSV output without loading the full input into memory." |
| "Will probably want to validate UTF-8." | "The tool MUST reject inputs containing bytes that are not valid UTF-8." |

Lowercase "must" and "should" in the produced spec are forbidden unless they appear inside a quoted external document.

## Status Line

Every spec MUST begin with a status line directly under the title. The literal format lives in [assets/spec-template.md](../assets/spec-template.md).

Common values: `Draft v1`, `Draft v2`, `Candidate`, `Ratified`. Qualifiers describe scope: `(language-agnostic)`, `(internal)`.

Bump the status only when a substantive change is approved. Cosmetic edits do not bump status.

## Implementation-Defined

Every spec MUST define "Implementation-defined" once, near the top. The exact definition wording lives in [assets/spec-template.md](../assets/spec-template.md).

Use this term to mark intentionally open policy choices where a single answer would be wrong.
It is not the same as "TBD" or "open question." Implementation-defined behavior is decided per implementation; an open question is undecided in general.

### When to use it

- Trust posture (sandboxing, approval policy) when the spec serves environments with different threat models.
- Resource limits where the right value depends on host.
- Logging sinks, dashboard layouts, and other observability surfaces with no interoperability requirement.

### When not to use it

- Anything observable across an integration boundary. Those MUST be specified.
- Vague handwaving where the author has not decided. That is an open question, not implementation-defined behavior.

## External Protocols

External standards (RFC 3986, RFC 4180, gRPC, etc.) MUST be referenced by name, not restated.

```markdown
The tool MUST parse CSV according to RFC 4180.
```

Not:

```markdown
The tool MUST treat double-quote as the quote character, doubled
double-quotes as the escape sequence, ... [restates RFC 4180]
```

If the spec appears to conflict with the named external standard, the external standard controls.

## No Changelog

The spec MUST NOT contain a changelog, revision history, or version table.
History lives in git.
Decision rationale lives in ADRs.

A "Status: Draft v2" line is acceptable; a "Changes from v1: ..." section is not.

## ADR Back-References

The spec MAY back-reference ADRs at decision points where the choice is non-obvious or contested:

```markdown
The orchestrator does not persist scheduler state across restarts
(see ADR-0042 for why; tracker-driven recovery was chosen over a durable queue).
```

Back-references MUST be sparse.
The spec MUST remain readable without following them.
A back-reference at every requirement is noise.

## Prose Style

- Short, declarative sentences. One requirement per sentence where possible.
- Active voice for requirements. "The tool MUST exit non-zero." Not "exit non-zero is required."
- Concrete identifiers in backticks. Configuration keys, file names, error categories, function names.
- Defaults inline with the field they belong to. No separate "Defaults" table.

## Forbidden

- "should" / "must" used as English synonyms of "ought to" or "has to."
- A separate defaults table when the keys already have descriptions.
- A "Future work" section masquerading as requirements.
- Vague modifiers in requirements: "robust", "performant", "user-friendly", "seamless".
  Replace with measurable criteria or delete.
- Implementation pseudocode placed inline with prose requirements.
  Pseudocode belongs in its own "Reference Algorithms" section.
