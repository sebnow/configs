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

## Level of Description: Contract vs Implementation

A spec describes the system's contract, not its implementation.
The same spec MUST be satisfiable by independent rewrites in different languages,
storage engines, and runtime hosts unless one of those choices is itself the contract.

The "contract surface" is what an integrator, operator, or downstream consumer can observe.
Everything else is an implementation choice the spec MUST stay agnostic to.

### On-contract — name exactly

- External protocols and standards (RFC 2119, RFC 3339, HTTP status codes, gRPC, GraphQL, SQL when SQL is the public interface).
- Configuration keys, environment variables, and file paths the operator sets (`WORKFLOW.md`, `POLL_INTERVAL_MS`).
- Wire-level identifiers exchanged with external systems (event type discriminators, JSON field names, header names, URL paths).
- Error category identifiers callers branch on (`missing_workflow_file`, `template_render_error`).
- File-format contracts when the file format crosses a boundary (front-matter schema, log line schema).
- Named integrations by their external name (the issue tracker, the agent runtime), not by the internal adapter module.

### Off-contract — describe by role, not by name

- Internal packages, modules, files, directories of the implementation (`pkg/foo`, `cmd/bar`, `internal/baz`).
- Language-specific types in field signatures (`uint16`, `BatchHandle`, a stdlib helper function name, a Go interface name, a Python class name).
- Database table names, column names, DDL types, constraints, and grants when the database is internal storage.
- Storage and infrastructure technologies (Postgres, Redis, S3, AWS Lambda) when they are internal coupling, not an external contract.
- Internal function and method names.

When a property must hold at an internal boundary,
describe it by what is true at the boundary
(e.g. "the event MUST be durably persisted before the cache is updated"),
not by the technologies on either side
(e.g. "the Postgres insert MUST precede the Redis update").

### Domain model field types

Field types in the domain model MUST use language-neutral types:

- `string`, `integer`, `boolean`, `timestamp`, `path`, `URL`, `UUID`
- `list of <T>`, `map of <K, V>`, `object`, `enum { a, b, c }`
- `null` or `<T> or null` for optionality

A field signed `uint16`, `int64`, or `Option<T>` is over-specified.

### Reference algorithms

Pseudocode in a Reference Algorithms section MUST be language-neutral.
No language-specific calls, types, or library names.
The algorithm describes the control flow, not the call sites.

### Before / After

| Implementation-bound | Contract-level |
|---|---|
| "`pkg/widgetlog` MUST expose `Record` with `Kind() RecordKind` and `Schema() uint8`." | "The producer MUST emit each record with a stable discriminator and a monotonic schema number." |
| "`record_kind` (TEXT) — discriminator, constrained by an allowlist `CHECK`." | "Each record carries a discriminator drawn from an allowlist enforced at the producer." |
| "`cmd/widget-ingest` writes one `widget_created` row per request before `cacheWidget`." | "For each request, the system MUST persist the `widget_created` record before updating the read cache." |
| "The row write and record write MUST ride one `db.WithTx` transaction." | "The row write and record write MUST be atomic with respect to one another." |

### When implementation detail is on-contract

Some specs are about an implementation surface — a CLI flag, an SDK type, a DB schema exposed to other systems.
In those cases the implementation detail IS the contract. Name it exactly.

Heuristic: if a competing implementation in a different language, framework, or storage engine
could satisfy the requirement, the requirement is at the right level.
If only the named technology can satisfy it, either the named technology is the contract
(say so) or the requirement is over-specified.

## Invariants vs Mechanisms

The previous section ruled out naming the technologies an internal component uses.
This section rules out specifying the coupling between internal components.

A spec states invariants — externally observable properties that hold of the system as a whole.
A spec MUST NOT state mechanisms — the ordering, transaction, cache, index, queue, or constraint
that the implementation chose to make those invariants hold.

If a requirement mentions "the cache", "the index", "the transaction", "the queue",
"the upsert", "the constraint", or a similar internal primitive,
it is mechanism-shaped.
The mechanism is one of several implementations that could preserve the same invariant.
Reduce the requirement to the externally observable property.

### Litmus tests

- Replace the named mechanism with "somehow". If the requirement still has meaning,
  the mechanism was incidental and the property survives. If the requirement becomes empty,
  it was specifying the mechanism, not a property.
- Ask: which boundary observation changes if the implementation uses a different mechanism?
  If none, the requirement is over-specified.

### Before / After

| Mechanism-shaped | Invariant-shaped |
|---|---|
| "The event MUST be persisted before the read cache is updated for the same request." | "No consumer MAY observe a request without its corresponding event." |
| "The upsert MUST return a flag distinguishing actual insert from conflict-update. The handler MUST enqueue the event only when the flag indicates an actual insert." | "For each entity, exactly one creation event MUST be emitted at the moment it first becomes durably persisted." |
| "Identifiers MUST be promoted to indexed columns derived from the payload, with cascade-delete from their parent." | "When a parent entity is removed, every event referencing it MUST also be removed atomically." |
| "The binding MUST be enforced via a non-null reference and a unique constraint." | (Drop. The 1:1 invariant is already the requirement; the constraint is one mechanism.) |
| "The row write and event write MUST be atomic with respect to one another." | "An accepted request either commits both its row and its event, or neither becomes observable." |

### Solutions disguised as requirements

Caching, indexing, transactions, batching, queueing, partitioning, and similar primitives
are solutions to performance, atomicity, and consistency properties.
They MUST NOT appear in the spec unless the externally observable property they serve
is also specified — in which case the spec specifies the property and treats the
solution as implementation-defined.

- A latency budget is a property; a cache is a solution. State the latency budget.
- An atomicity scope at a boundary is a property; a transaction is a solution. State the scope.
- An ordering or consistency guarantee at a boundary is a property; serializable isolation
  is a solution. State the guarantee.
- A throughput or fairness target is a property; a queue is a solution. State the target.

If the spec does not specify the property a solution serves, the solution does not belong
in the spec.

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
- Internal package, module, file, or directory paths of the implementation
  (`pkg/foo`, `cmd/bar`, `internal/baz`) used as canonical identifiers in normative prose.
  Describe the component by its role.
- Language-specific types in field signatures (`uint16`, `BatchHandle`, a Go interface name).
  Use the language-neutral types listed under "Level of Description".
- DDL, table names, column names, or grants when the database is internal storage.
  State the data shape and invariants instead.
- Internal infrastructure technologies (Postgres, Redis, S3, a queueing service)
  named in requirements about internal coupling.
  Describe the boundary property without naming the technologies.
- Internal function or method names in conformance criteria or test descriptions.
  State the observable behavior at the system boundary instead.
- Mechanism-shaped requirements that specify internal coupling
  (cache ordering, transaction boundaries, upsert flags, index promotion, constraint shape)
  when an externally observable invariant would serve.
  See "Invariants vs Mechanisms".
- Solution-shaped non-functional requirements (caching, indexing, batching, queueing)
  appearing without the externally observable property they serve.
  Specify the property; treat the solution as implementation-defined.
