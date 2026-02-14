---
name: coding
description: "Use when implementing code changes. Enforces production-grade principles: pragmatism, domain awareness, clarity, performance, correctness. Triggers: implementing features/fixes, refactoring, writing new code, bug fixes."
---

# Coding Practices

## Core Principles

Apply these principles to every implementation:

- Pragmatism First:
  Choose solutions that work reliably in practice over theoretically perfect approaches.
  Favor battle-tested patterns and libraries.
  Avoid premature optimization and over-engineering.
  For dependencies,
  prefer small bits of duplication over adding third-party dependencies for trivial functionality.
  Larger or complex functionality requires careful trade-off consideration - discuss when uncertain.
- Domain Aware:
  Every system has purpose and intrinsic concepts - compiler has parsers and ASTs,
  game has players and inventories,
  financial system has transactions and ledgers.
  Use terminology and concepts fundamental to what the system does,
  rather than generic technical terms or made-up names.
  Ensure changes are cohesive with system's purpose and broader project.
  Don't use "best practices" or design patterns for the sake of it - make sure they're fit for purpose.
- Clarity and Legibility:
  Write code that communicates intent clearly.
  Use straightforward control flow - avoid unnecessary jumping around that loses context.
  Prefer larger functions if context would be lost by extracting shallow functions.
  Code should be self-documenting wherever possible.
- Performance:
  Choose appropriate algorithms and data structures from start - don't use O(n²) when O(n) is obvious,
  don't iterate through arrays when hash map is natural fit.
  This is about sound engineering defaults,
  not micro-optimization.
  Measure before doing actual optimization work.
  Be conscious of systems programming concepts like memory alignment and padding.
- Correctness:
  Ensure code handles edge cases,
  validates inputs,
  and fails gracefully.
  Error handling is not an afterthought - consider at architecture level from start.
  Think through error conditions and boundary cases before writing.
  There are more edge cases than happy paths - design solution with this reality in mind.

## Implementation Workflow

Follow this workflow for all code changes:

1. Verify Project Health:
   Before starting any implementation,
   verify the project is in a clean state.
   Run existing tests to establish baseline
   (follow red-green-refactor skill).
   If tests fail,
   fix them first or confirm with user they should be ignored.
   Never start new work with failing tests.
2. Understand Requirements:
   If ambiguous,
   ask clarifying questions.
   Don't assume - verify.
   Consider project documentation (README,
   CONTRIBUTING,
   architecture docs) as requirements.
3. Design Before Coding:
   Think through approach,
   data structures,
   algorithms.
   Consider trade-offs between simplicity and performance.
4. Verify APIs:
   Confirm library APIs before use - don't guess at function signatures or behavior.
   Use language documentation tools: `go doc` (Go),
   `man` (C),
   `cargo doc` (Rust),
   `pydoc` or `help()` (Python).
5. Write Incrementally:
   Build up functionality in logical steps.
   Test assumptions as you go.
6. Self-Review:
   Review for edge cases/error handling,
   performance bottlenecks (obvious ones),
   clarity (can someone else understand quickly?),
   adherence to project patterns/conventions.

## Code Style

- Comments:
  Add only when providing additional context or explaining non-obvious decisions.
  Don't add superfluous comments that merely restate what code does.
- Naming:
  Use domain terminology.
  Names should imply type - prefixes like `is_` or `did_` for booleans,
  `count` for numbers,
  units in names like `timeoutMs` unless type system enforces it (e.g.,
  Go's `time.Duration`).
  Prefer relevant,
  verbose array subscript names over one-letter variables like `i`,
  `j`,
  `k` - at least use `idx` for clarity.
  Avoid Hungarian notation and type-encoding schemes.
  Avoid abbreviations unless domain-standard.
- Structure:
  Group related functionality together.
  Keep functions focused on single responsibility,
  but prefer larger functions when extracting shallow functions would lose important context.
- Error Handling:
  Errors are first-class architectural concern.
  Handle errors,
  don't just propagate them.
  Consider whether error requires alternative path,
  default value,
  or actual failure.
  When errors cross abstraction boundaries,
  transform them to match abstraction level - "database row not found" becomes "entity not found" at domain layer,
  "file read error" becomes "configuration load error" at application layer.
  Implementation details must not leak through error types.
  Fail fast only on truly unrecoverable errors.
- Language-Specific: Follow established conventions for the language you're writing in.

## Boy Scout Rule

Improve code you touch opportunistically - fix obvious issues in files you're modifying.
Separate refactoring commits from feature commits.
Defer larger refactors unless explicitly requested.
Note technical debt for later rather than fixing during unrelated work.

## Context Awareness

Always consider:
- Project documentation (README, CONTRIBUTING, architecture docs, API documentation)
- Project coding standards and conventions
- Established patterns and architectural decisions
- Language-specific guidelines provided in project context

When these conflict with general best practices,
prioritize project-specific instructions.

When implementing features or fixes,
check for relevant documentation that should be updated to reflect changes.

## Quality Standards

Before delivering code:
1. Verify it compiles/runs (if applicable)
2. Ensure error paths are handled
3. Confirm it follows project conventions

If uncertain about any aspect of implementation,
state assumptions clearly and ask for confirmation rather than guessing.

Goal: Deliver code that works correctly,
performs well,
and can be easily understood and maintained by other developers.
