---
name: coding
description: "Use when implementing code changes. Enforces production-grade principles: pragmatism, domain awareness, clarity, performance, correctness. Triggers: implementing features/fixes, refactoring, writing new code, bug fixes. Go hot-rule: layout-compatible structs cross layer boundaries with `T(other)`, never field-by-field — `T(other)` is a compile-time-checked zero-cost conversion, not unsafe and not reflection."
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
- Discover Abstractions, Don't Invent Them:
  Write concrete code that solves the specific problem first.
  Do not introduce abstractions, indirection, or generalization
  until the same logic has appeared in at least two places.
  Before creating any interface, wrapper struct, generic function,
  or helper, verify it passes all of these checks:
  (a) the interface has at least two implementations that exist now,
  (b) the struct has at least two fields or two call sites,
  (c) the function is called from at least two sites,
  (d) the extraction uses concrete types — no type parameters
  unless both call sites require different types.
  If any check fails, inline the code instead.
  A struct wrapping a single dependency with one method
  is a closure in disguise — use a closure or standalone function.
  "Callers will likely want to" is not a justification —
  extract when callers actually do, not when they might.
  When compressing duplicated code,
  extract the literally repeated lines into one function
  returning the most concrete type possible.
  Do not decompose extracted helpers into further sub-functions.
  Do not add methods, parameters, or features beyond what was requested.
- Domain Aware:
  Every system has purpose and intrinsic concepts - compiler has parsers and ASTs,
  game has players and inventories,
  financial system has transactions and ledgers.
  Use terminology and concepts fundamental to what the system does,
  rather than generic technical terms or made-up names.
  Ensure changes are cohesive with system's purpose and broader project.
  Don't use "best practices" or design patterns for the sake of it - make sure they're fit for purpose.
  Typed primitives at boundaries:
  at public function boundaries,
  primitive values that carry semantic constraints
  (a user reference, a tenant reference, a URL, an email)
  get a named type with a `Parse*` (or `New*`) constructor
  that takes the raw input and returns the named type and an error.
  The validation lives in the constructor;
  functions inside the boundary accept the named type and trust the value.
  Name the type even when the constructor does not validate anything yet —
  naming documents the meaning at the signature,
  and the named type prevents silently swapping two distinct identifiers at the call site.
  Do not introduce a named type for a value with no current or foreseeable semantic rule
  (e.g. a free-form correlation ID).
- Push Invariants Into the Data Shape:
  When the domain forbids a state, change the data so the state is unrepresentable —
  do not guard against it in code.
  If a pointer field is documented or constrained to be always-set
  (a schema check, a loader invariant, an "every X has a Y" comment),
  change its declared type to the value type — drop the pointer from the struct.
  Dereferencing the pointer without a nil check is not a workaround for the bad shape;
  it leaves the bad shape in place and silently relies on the invariant holding forever.
  Same move for queries that return rows the caller must filter out:
  change the query (e.g. `LEFT JOIN` → `INNER JOIN`) rather than filtering in code.
  Same for fields that are always set: make them required rather than checking the zero value.
  The smell is not the runtime check — the smell is the type permitting a state the runtime never produces
  (a nullable always set, an enum value that never appears, a slice that is never empty).
  Whether the code currently has an `if x != nil` guard, a naked dereference, or nothing at all
  is irrelevant: if the type allows the forbidden state, the data shape is wrong.
  When proposing this change, name the principle:
  the goal is to push the invariant into the data shape so the code can't be wrong,
  not merely to write "simpler" or "cleaner" code.
- Layering Direction:
  Orchestration and decisions live in business code;
  infra primitives execute.
  Business code calls infra through interfaces it owns;
  infra never invokes business policy in return.
  Retry, fallback, and conditional flow decisions belong in the business
  caller — infra reports the outcome and the caller decides what to do.
  Smell check: if an adapter or repository contains a retry policy,
  decides when to fall back, or sequences workflow steps,
  the decision belongs in the use case, not the adapter.
  Infra implementing a domain interface is normal — that is how Clean
  Architecture inverts the dependency. The smell is logic flowing the
  other way: a use case reaching past its port into an adapter's concrete
  type, or an adapter encoding business rules.
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
4. Plan Implementation Steps:
   Before writing code,
   enumerate the independent logical changes needed.
   Separate mechanical changes (renames, moves, dead code removal)
   from functional changes (new behaviour, bug fixes).
   State the planned sequence of commits.
   Each step must be independently committable and testable.
5. Verify APIs:
   Confirm library APIs before use - don't guess at function signatures or behavior.
   Use language documentation tools: `go doc` (Go),
   `man` (C),
   `cargo doc` (Rust),
   `pydoc` or `help()` (Python),
   `zigdoc` (Zig — drill iteratively: `zigdoc std.ArrayList` then `zigdoc std.ArrayList.init`).
6. Implement, Test, Commit — One Step at a Time:
   Implement one planned step.
   Verify it compiles and tests pass.
   Commit following the commit skill.
   Only then proceed to the next step.
   Never accumulate multiple logical changes before committing.
7. Self-Review:
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
  transform them to match abstraction level — "database row not found" becomes "entity not found" at domain layer,
  "file read error" becomes "configuration load error" at application layer.
  Name errors by consequence to the caller —
  what they lost, not what broke internally.
  Distinguish permanent domain conditions (the entity does not exist)
  from temporary infrastructure failures (the operation could not complete) —
  callers need to tell these apart to decide whether to retry or give up.
  Implementation details must not leak through error types.
  Errors are control flow: `err != nil` means the operation failed.
  Supplementary success-path information (e.g. a cache miss on a successful fetch) is not an error —
  use a separate mechanism if callers need to act on it.
  If the caller has no meaningful branch on failure, panic;
  returning an error the caller cannot act on forces artificial handling at every call site.
- Language-Specific: Follow established conventions for the language you're writing in.
  For Go modern APIs (1.26+), see [go-modern-apis.md](references/go-modern-apis.md).
  For other Go-specific guidance, see [go.md](references/go.md).
  For Zig concurrency and threading, see [zig-concurrency.md](references/zig-concurrency.md).
- Go-style idioms (when writing Go):
  - When two structs share an identical field layout (same names, same types,
    same order), convert between them with the type-conversion expression
    `T(other)` rather than constructing a literal field-by-field.
    `T(other)` is a compile-time-checked, zero-cost conversion — not unsafe,
    not reflection. If the layouts later diverge, the cast stops compiling
    at exactly the boundary that needs to know; that is the signal to map
    explicitly. Field-by-field is the fragile option, not the safe one:
    it silently keeps compiling when only one side gains a field. Apply
    this idiom even when the types are documented as "may diverge later" —
    let the compiler tell you when "later" arrives.
    See [go.md](references/go.md) for examples.

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
