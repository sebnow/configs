---
name: testing
description: "Use when implementing automated tests. Enforces empirical testing, test pyramid, TDD practices, concurrent testing. Triggers: writing tests, test planning, implementing test cases, TDD, race conditions."
---

# Testing

## Core Principles

Automated testing validates that software meets requirements and behaves correctly.
Tests must be fast,
reliable,
and deterministic.

You must:
- Test through execution, not speculation
- Tie each test to a requirement or behavior
- Focus on observable behavior, not implementation details
- Make tests maintainable and refactor-friendly
- Treat tests as documentation of system behavior

## Testing Pyramid for Systems Software

Always test at the right level:

Unit (base - most numerous):
Fast isolated tests of single components.
Run in microseconds to milliseconds.
No external dependencies.
Include property-based tests for algorithms.

Integration (middle - substantial):
Verify component interactions.
Database, network, filesystem interactions.
Run in milliseconds to seconds.
Include contract tests between services.

System (higher):
Full system behavior under load.
Performance and resource usage validation.
Run in seconds to minutes.

Chaos/Resilience (top):
Failure injection and recovery.
Network partitions, resource exhaustion.

## Test Priorities

Every time you write tests,
prioritize by risk and value:

Critical (test first):
Core workflows,
auth/authz,
data integrity,
payments/transactions,
error handling/recovery,
security vulnerabilities

Important (test thoroughly):
Edge cases in business logic,
input validation,
performance benchmarks,
integration points,
concurrent operations

Low (test if time permits):
Cosmetic issues (unless breaking usability),
minor inconsistencies,
theoretical edge cases without clear risk

## Error and Failure Testing

Errors are more common than happy paths.
You must test error conditions explicitly:

Required error tests:
- Invalid inputs and boundaries
- Network failures and timeouts
- Database connection errors
- File system errors (permissions, disk full)
- Concurrent access and race conditions
- Resource exhaustion
- Graceful degradation
- Error message clarity

Never assume error paths work without testing them.

## Table-Driven Tests

Use table-driven tests only for simple cases where the same logic checks different inputs.

Never use conditional logic based on test case.
Patterns like `wantErr` fields are red flags.

When test cases need different logic (success vs error, different validations),
write separate test functions instead.

See @go-patterns.md for examples.

## Test Fidelity Tier

When a test needs to substitute a collaborator,
choose the highest-fidelity option that fits the layer under test:

1. Real thing (highest fidelity).
   Adapter and repository tests run against the real dependency:
   real Postgres for repository tests,
   real HTTP server (`httptest.NewServer`) for HTTP clients,
   real filesystem under `t.TempDir()` for file-system code.
   Pair with build tags (`//go:build integration`) when the dependency
   is heavyweight.
2. Generated mock at the substitution boundary (when real is infeasible).
   Use a code generator — `moq`, `mockgen`, or equivalent — driven by a
   `//go:generate` directive next to the interface declaration.
   Generated mocks regenerate when the interface changes,
   so call-count assertions and signatures cannot drift silently.
   This is the default at the activity / use-case / handler layer
   where the real adapter would require external infrastructure.
3. Hand-rolled fake (last resort).
   Only when neither real nor generated will work
   — for example, an in-memory implementation that owns nontrivial state
   the test must inspect.
   A hand-rolled struct that just records calls is a generated-mock
   in disguise; reach for the generator instead.

A hand-written `type fake<Name> struct` with method receivers implementing an
interface, used only to record calls or return canned values, is the
anti-pattern. Replace it with a generated mock.

## Parallel Test Isolation Ordering

Adding `t.Parallel()` to a test that shares mutable state with another test
introduces a race, not a speedup. A flaky test under parallelism is a
race-condition bug. Fix the isolation; do not retry, skip, or sleep around it.

When a test in a package needs to run in parallel, follow this order:

1. Namespace the test fixtures.
   Per-test stub keys, per-test temp directories (`t.TempDir()`),
   per-test HTTP servers (`httptest.NewServer`), or per-test database schemas.
   Each test owns its own fixture state.
2. Remove shared mutable state.
   Convert package-globals to per-test instances.
   Inject collaborators through function or struct parameters
   instead of reading them from a global.
3. Add `t.Parallel()` to the test functions that are now isolated.
4. Drop `-p 1` (or any other serial-execution config) from the test runner.

Steps 1 and 2 are non-negotiable preconditions for step 3.
A test that still touches a package-global is not a candidate for `t.Parallel()`,
no matter how urgent the CI-time pressure is.

## Testing Concurrent Code

Concurrency must be tested explicitly.

Required practices:
- Always run tests with `-race` flag during development
- Test goroutine lifecycles and proper cleanup
- Verify channels don't deadlock
- Test synchronization primitives
- Check for goroutine leaks

See @go-patterns.md for race detection examples.

## Test Context (Go 1.24+)

Use `t.Context()` as the base context in all Go test code.
Never use `context.Background()` or `context.TODO()` in tests.
When deriving contexts (deadlines, cancellation, values),
use `t.Context()` as the parent.

See [go-patterns.md](go-patterns.md) for examples.

## Testing I/O Operations

Database testing:
- Use transactions with deferred rollback for test isolation
- Test migration up and down
- Verify connection error handling

Network testing:
- Use `httptest.NewServer` for HTTP handlers
- Test with random ports for parallel execution
- Set deadlines to prevent hanging tests
- Test timeout and retry logic

File system testing:
- Use `t.TempDir()` for automatic cleanup
- Test permission errors
- Verify symlink handling
- Test file watching with timeouts

## Test Organization

Package structure:
- Use `package name_test` for black-box testing (default)
- Use `package name` only when testing internals
- Create `export_test.go` to expose internals when needed

Use build tags to separate integration tests from unit tests.

## Test Helpers

Mark helper functions with `t.Helper()` to report errors at call site.

## Property-Based Testing

Use property-based testing for algorithms.
Test invariants across many inputs rather than specific cases.

## Benchmarking

Required practices:
- Use `b.ResetTimer()` to exclude setup
- Use `runtime.KeepAlive()` to prevent compiler optimization
- Use subtests for multiple sizes
- Report custom metrics when relevant

See @go-patterns.md for examples.

## TDD Workflow

Follow red-green-refactor skill for the core TDD methodology.

When practicing TDD:

1. Red: Write failing test that defines desired behavior
2. Green: Write minimal code to make test pass
3. Refactor: Improve code while keeping tests green
4. Verify: Run full test suite including race detection

Required: Run test before and after implementation to verify red-green cycle.

## Test Quality Standards

Before considering tests complete:
- All tests pass consistently (no flakiness)
- Race detector passes: `go test -race`
- Tests run quickly (parallelize when possible)
- Error messages clearly indicate what failed
- No hardcoded timeouts that cause flakiness
- Proper cleanup in all paths (use `defer` and `t.Cleanup()`)

Goal: Create reliable,
fast,
maintainable tests that validate requirements and enable confident refactoring.
