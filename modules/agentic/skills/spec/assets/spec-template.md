<!--
Canonical skeleton for a produced SPEC.md. This template is the single source
for section shape, when-to-include criteria, and per-section guidance.

Required sections are present and ordered.
Conditional sections appear as commented placeholders; include each only when
its when-to-include criterion is met. Delete the placeholder otherwise — do
not leave an empty section.

If an input warrants a section type not in this template, propose adding it
to the template before emitting it.

Strip every HTML comment from the final document before writing.
-->

<!--
Selection heuristics — guides, not rules:

| Input shape                       | Likely sections                                                                                                              |
|-----------------------------------|------------------------------------------------------------------------------------------------------------------------------|
| Stateless library (e.g. parser)   | Problem, Goals/Non-Goals, Public API/Domain Model, Error Categories, Tests, DoD                                              |
| CLI tool with config              | Problem, Goals/Non-Goals, CLI Contract, Config, Error Categories, Tests, DoD                                                 |
| Long-running service with state   | Problem, Goals/Non-Goals, Components, Domain Model, State Machine, Polling/Scheduling, Failure Model, Logging, Pseudocode, Invariants, Tests, DoD |
| Library + service split           | Both patterns above, partitioned into profiles                                                                                |
-->

# Spec: <project name>

<!-- Purpose: identify the document and its standing. -->

Status: Draft v1<!-- bump only on substantive change; (qualifier) optional, e.g. (language-agnostic) -->

## Normative Language

<!-- Purpose: bind the RFC 2119 keywords. See references/voice-guide.md for the wording requirements. -->

The key words MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY, and OPTIONAL in this document are to be interpreted as
described in RFC 2119.

## Implementation-Defined

<!-- Purpose: define the term used to mark intentionally open policy choices. See references/voice-guide.md. -->

`Implementation-defined` means the behavior is part of the implementation
contract, but this specification does not prescribe one universal policy.
Implementations MUST document the selected behavior.

## Problem Statement

<!--
Purpose: one paragraph stating what the system is and the problem it solves.
No history, no comparisons, no rationale. Present tense.
-->

## Goals

<!-- Purpose: scope fence (positive half). One-line goals, one per bullet. -->

- <goal>

## Non-Goals

<!--
Purpose: scope fence (negative half).
At least one non-trivial exclusion required.
"Not <obvious feature>" is a valid non-goal when it heads off scope creep.
-->

- <non-goal>

<!--
========================================================================
Conditional sections below. Include each only when its when-to-include
criterion is satisfied by the inputs or existing spec. Omit otherwise.
Delete this entire block in the final document.
========================================================================

## System Overview / Components
   When: the system has more than one logical component or layer.
   Skip for single-module libraries; otherwise this section is RECOMMENDED
   and SHOULD precede the Core Domain Model so readers see the role
   structure before the data structures.
   Name each component by its role (for example "Workflow Loader",
   "Request Validator", "Storage Adapter"), never by an internal
   module, package, file, or directory path. See references/voice-guide.md
   "Level of Description".
   For each component: one-line responsibility, what it owns, what it
   depends on. No implementation references.
   MAY include a sibling "Abstraction Levels" subsection naming the
   architectural layers (policy / configuration / coordination /
   execution / integration / observability or equivalent) when the
   system spans several.

## Core Domain Model
   When: the system has data structures or entities shared across components.
   Each entity entry: name, fields with types, normalization rules.

## State Machine
   When: the system has stateful coordination (running/queued/retrying, lifecycle phases).
   Skip for stateless tools.
   Each state: name, entry conditions, exit conditions, transitions.

## Workflow / Configuration Specification
   When: the system reads a configuration file or in-repo workflow definition.
   Each config key: type, default (inline with the key, never a separate defaults table),
   validation rules, dynamic-reload semantics if applicable.

## Polling / Scheduling / Reconciliation
   When: the system polls an external source or schedules work on intervals.

## Workspace / Filesystem Safety
   When: the system isolates work in per-task directories or has filesystem boundary constraints.
   Include named numbered invariants for the safety properties.

## Protocol Integration Contract
   When: the system integrates an external protocol (HTTP, gRPC, GraphQL, an app-server).
   Identify the external protocol as the source of truth; specify only the orchestration the spec owns.
   Reference the external standard; do not restate it.

## Prompt Construction and Context Assembly
   When: the system constructs LLM prompts.
   Cover input variables, rendering rules, failure semantics, retry/continuation semantics.

## Logging, Status, and Observability
   When: operators need runtime visibility.
   Cover required context fields, formatting rules, sinks (without prescribing one), optional snapshot interfaces.

## Failure Model and Recovery
   When: failure handling is non-trivial.
   Cover failure classes, recovery behavior per class, restart recovery, operator intervention points.

## Security and Operational Safety
   When: the system has trust boundaries, secrets, or privileged operations.
   Cover trust posture (often implementation-defined), filesystem safety, secret handling, hardening guidance.

## Named Numbered Invariants
   When: the system has safety or correctness properties that MUST hold.
   Numbered, named, referenceable. See references/structural-elements.md for preservation rules
   (never renumber existing invariants on update).
   Format:
     Invariant 1: <one-line property>.
     - <precondition or check>.
     - <enforcement>.

## Named Error Categories
   When: distinct error classes need to be reasoned about by callers, tests, or operators.
   Each name is a stable identifier callers can branch on. New categories may be added across
   edits; existing categories MUST NOT be renamed. See references/structural-elements.md.
   Format:
     - `error_category_name`

## Reference Algorithms (Pseudocode)
   When: non-trivial control flow (orchestration loops, retry logic, reconciliation)
   benefits from one concrete realization for implementer guidance.
   Language-agnostic pseudocode in fenced code blocks. Function-level granularity.
   Each algorithm referenced from the prose section it implements.
   The algorithm is one mechanism that satisfies the spec's invariants.
   An alternative mechanism MAY satisfy the same invariants.
   The pseudocode is illustrative; the invariants in the prose control conformance.
   Pseudocode MUST NOT use storage-specific or language-specific primitives
   (SQL clauses, language-stdlib function names) when an abstract operation
   conveys the same control flow.

## Extension Appendices
   When: the spec defines OPTIONAL features that an implementation MAY ship.
   Each extension gets its own appendix, with its own conformance section.
========================================================================
-->

## Test and Validation Matrix

<!--
Purpose: enumerate the deterministic conformance checks an implementation MUST pass.
Each test references the prose section it verifies. Required even for small specs.

Conformance profiles: when the spec defines OPTIONAL extensions, partition into
  - `Core Conformance` — REQUIRED of every implementation.
  - `Extension Conformance` — REQUIRED only for implementations that ship the extension.
  - `Real Integration Profile` (OPTIONAL) — environment-dependent smoke checks.
For minimal specs with no extensions, a single un-named profile is acceptable.
-->

- §<section>: <observable behavior>

## Implementation Checklist (Definition of Done)

<!--
Purpose: enumerate what an implementation MUST ship to claim conformance.
Mirrors the test matrix profile structure.
Each checkbox enumerates a deliverable an implementation must ship.
-->

- [ ] <deliverable>
