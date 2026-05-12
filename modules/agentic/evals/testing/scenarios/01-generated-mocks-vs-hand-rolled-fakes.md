---
id: 01-generated-mocks-vs-hand-rolled-fakes
title: Substitute repository via generated mock, not hand-rolled fake
fixture: fixtures/generated-mock-substitution
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `IssueAPIKey` depends on an `APIKeyRepository` interface so the database
  can be substituted in unit tests. The repo's convention is to generate
  mock implementations with `moq` from `//go:generate` directives — never to
  hand-roll a fake struct that implements the interface. Hand-rolled fakes
  drift from the interface as methods are added, hide call-count assertions
  behind ad-hoc fields, and duplicate effort across the codebase. With the
  fidelity-tier rule encoded in the testing skill, the agent should reach
  for a `moq` directive (or the equivalent generated-mock workflow) instead
  of writing a fake by hand. Without the rule the path of least resistance
  is a `type fakeAPIKeyRepo struct { ... }` declaration in the test file.
assertions:
  - id: generated-mock-proposed
    text: |
      The agent proposes a generated mock for `APIKeyRepository` — concretely,
      it adds (or names) a `//go:generate ... moq` directive, references
      `moq` / `mockgen` / `gomock` / `counterfeiter`, or runs `go tool moq`
      to produce the substitution. Naming the tool counts; producing the
      generated file in addition is acceptable but not required.
  - id: no-hand-rolled-fake
    text: |
      The agent does NOT write a hand-rolled fake struct that implements
      `APIKeyRepository` by declaring its own struct type with method
      receivers (e.g. `type fakeAPIKeyRepo struct { ... }` followed by
      `func (f *fakeAPIKeyRepo) Create(...) error { ... }`). Using the
      generated `APIKeyRepositoryMock` value (with `CreateFunc` etc.)
      satisfies this assertion.
  - id: rationale-named
    text: |
      The agent's reasoning explicitly references the fidelity tier or the
      hand-rolled-fake anti-pattern (e.g. "prefer generated mocks over
      hand-rolled fakes", "drift", "regenerate when the interface
      changes"). Merely producing the right artifact without explaining
      why does not satisfy this assertion.
---

Add a unit test for `IssueAPIKey` in `api_key.go`. Cover both the happy path
and the error path returned by the repository. There is no Postgres available
here — the test must substitute `APIKeyRepository`. Choose the substitution
mechanism that matches this codebase's conventions.
