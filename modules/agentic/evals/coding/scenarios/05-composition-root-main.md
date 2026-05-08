---
id: 05-composition-root-main
title: main.go is the composition root, no DI container library
fixture: fixtures/composition-root-main
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  The user prefers plain Go composition in `main.go` (or its delegate
  `run`) over reflection-based DI container libraries (uber/dig,
  uber/fx, google/wire, samber/do, facebookgo/inject, and similar).
  The composition root is just code: declare deps as variables, pass
  them to constructors, return errors normally. Agents already lean
  this way in idiomatic Go, and this scenario is a regression lock —
  baseline PASSes without skill text. Commit captures the expected
  behavior so a future SKILL.md edit cannot accidentally drift agents
  toward DI-container shapes.
assertions:
  - id: plain-composition
    text: |
      The agent's implementation of `run` constructs each dependency as
      a plain Go variable (e.g. `db := NewDatabase()`,
      `cache := NewCache()`) and passes them to the next constructor as
      explicit arguments. The body is straight-line code, not a
      container/registry/locator abstraction.
  - id: no-di-library-import
    text: |
      The agent does not introduce any import for a dependency-injection
      container library. Forbidden imports include (but are not limited
      to): `go.uber.org/dig`, `go.uber.org/fx`,
      `github.com/google/wire`, `github.com/samber/do`,
      `github.com/facebookgo/inject`, or any package whose role is
      reflection-based dependency wiring. Standard-library imports
      (`context`, `log`, etc.) are fine.
  - id: explicit-dep-arguments
    text: |
      Every constructor call passes its declared parameters as explicit
      Go expressions in the call site (positional arguments to
      `NewRepository`, `NewService`, etc.). The agent does not introduce
      a service-locator pattern, a global registry, or a `Resolve(name)`
      lookup that pulls dependencies by string key.
---

Implement the body of `run` in `main.go`. It must build a `*Service` from
the available constructors (`NewDatabase`, `NewCache`, `NewEventBus`,
`NewRepository`, `NewService`) and then invoke `svc.Run(ctx)`. Propagate
the result of `svc.Run` as the return value of `run`.
