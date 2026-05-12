---
name: testing
description: "Use when implementing automated tests. Enforces empirical testing, test pyramid, TDD practices, concurrent testing. Triggers: writing tests, test planning, implementing test cases, TDD, race conditions. Go-specific patterns and rules: see references/go.md."
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

See [go.md](references/go.md) for examples.

## Test Fidelity Tier

When a test needs to substitute a collaborator,
choose the highest-fidelity option that fits the layer under test:

1. Real thing (highest fidelity).
   Adapter and repository tests run against the real dependency:
   real database for repository tests,
   real HTTP server for HTTP clients,
   real filesystem under a temp directory for file-system code.
   Pair with build tags when the dependency is heavyweight.
2. Generated mock at the substitution boundary (when real is infeasible).
   Use a code generator driven by the interface declaration.
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

See [go.md](references/go.md) for Go-specific tools and the hand-rolled fake anti-pattern.

## Parallel Test Isolation Ordering

Adding parallelism to a test that shares mutable state with another test
introduces a race, not a speedup. A flaky test under parallelism is a
race-condition bug. Fix the isolation; do not retry, skip, or sleep around it.

When a test in a package needs to run in parallel, follow this order:

1. Namespace the test fixtures.
   Per-test stub keys, per-test temp directories,
   per-test HTTP servers, or per-test database schemas.
   Each test owns its own fixture state.
2. Remove shared mutable state.
   Convert package-globals to per-test instances.
   Inject collaborators through function or struct parameters
   instead of reading them from a global.
3. Mark the test functions as parallel once they are isolated.
4. Drop any serial-execution configuration from the test runner.

Steps 1 and 2 are non-negotiable preconditions for step 3.
A test that still touches a package-global is not a candidate for parallel execution,
no matter how urgent the CI-time pressure is.

See [go.md](references/go.md) for Go-specific parallel isolation APIs.

## Testing Concurrent Code

Concurrency must be tested explicitly.

Required practices:
- Enable race detection during development
- Test concurrent task lifecycles and proper cleanup
- Verify blocking primitives don't deadlock
- Test synchronization primitives
- Check for resource leaks

See [go.md](references/go.md) for race detection examples.

## Testing I/O Operations

Database testing:
- Use transactions with deferred rollback for test isolation
- Test migration up and down
- Verify connection error handling

Network testing:
- Use an in-process HTTP server for HTTP handlers
- Test with random ports for parallel execution
- Set deadlines to prevent hanging tests
- Test timeout and retry logic

File system testing:
- Use temp directories with automatic cleanup
- Test permission errors
- Verify symlink handling
- Test file watching with timeouts

## Test Organization

Default to black-box testing (test via the public API).
Use white-box testing only when testing internals requires it.

Use build tags or equivalent to separate integration tests from unit tests.

See [go.md](references/go.md) for Go-specific package structure.

## Test Helpers

Mark helper functions so errors are reported at the call site, not inside the helper.

## Property-Based Testing

Use property-based testing for algorithms.
Test invariants across many inputs rather than specific cases.

## Benchmarking

Required practices:
- Exclude setup time from benchmark measurements
- Prevent the compiler from optimizing away benchmark code
- Use subtests for multiple sizes
- Report custom metrics when relevant

See [go.md](references/go.md) for examples.

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
- Race detector passes
- Tests run quickly (parallelize when possible)
- Error messages clearly indicate what failed
- No hardcoded timeouts that cause flakiness
- Proper cleanup in all paths

Goal: Create reliable,
fast,
maintainable tests that validate requirements and enable confident refactoring.
