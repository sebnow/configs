---
name: testing
description: "Use when implementing automated tests. Enforces empirical testing, requirements-driven validation, TDD practices. Triggers: writing tests, test planning, implementing test cases, TDD."
---

# Testing Practices

## Core Discipline

Validate software through systematic automated testing.
Tests must prove correctness through execution,
not speculation.

Every time you write tests:
- Test Empirically:
  Write tests that execute and verify behavior,
  not comments about what should work
- Requirements-Driven:
  Test against functional and non-functional requirements,
  prioritize based on risk/value
- Evidence-Based:
  Tests must fail when broken,
  pass when correct

## Test Strategy

Choose test types based on what you're validating and risk:

Unit:
Individual functions/methods in isolation - business logic,
edge cases,
pure functions,
fast feedback

Integration:
Component/system interactions - database ops,
API integrations,
message queues,
component boundaries

End-to-End:
Complete user workflows - critical journeys,
full stack,
business processes,
deployment configs

Property-Based:
Invariants across many inputs - parsers/serializers,
mathematical properties,
edge case discovery,
state machines

Test Doubles (use when needed):
- Mocks: Verify interactions (was API called?)
- Stubs: Predetermined responses
- Fakes: Simplified working implementations (in-memory DB)

Prefer real dependencies when practical.
Use doubles for: unreliable or expensive external dependencies,
error condition testing,
necessary isolation.

## Test Priorities

Always focus on high-risk,
high-value areas:

Critical (highest priority):
Core workflows,
auth/authz,
data integrity (CRUD),
payments/transactions,
error handling/recovery,
security

Important (medium):
Edge cases in business logic,
input validation,
performance benchmarks,
integration points (APIs/databases/filesystems),
concurrent operations

Low:
Cosmetic issues (unless breaking usability),
minor inconsistencies,
theoretical edge cases without clear risk

## Error and Failure Testing

Errors are more common than happy paths.
You must test explicitly:
- Invalid inputs/boundaries
- Network failures/timeouts
- Database connection errors
- File system errors (permissions/disk full)
- Concurrent access/races
- Resource exhaustion
- Graceful degradation
- Error message clarity

## Performance Testing

When non-functional performance requirements exist,
consider:
- Baseline metrics
- Consistency across runs
- Relevant metrics (response time, throughput, memory, CPU)
- Comparison against requirements

Goal: Validate requirements through systematic,
empirical testing that can be run repeatedly.
