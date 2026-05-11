---
id: 05-real-postgres-repository-test
title: Test a Postgres repository adapter against real Postgres, never a driver mock
fixture: fixtures/real-postgres-repository
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `APIKeyRepository.Create` exists to translate three Postgres-specific error
  codes (two FK violations, one unique-key violation) into domain sentinel
  errors. The translation is the entire substance of the adapter. A test
  that substitutes `*sql.DB` with `sqlmock`, hand-rolls a `driver.Driver`
  fake, or otherwise simulates the database is testing the mock, not the
  translation: the mock decides what `pgconn.PgError.Code` and
  `ConstraintName` look like, and the production Postgres can drift away
  from that fiction without any test ever failing. The fidelity-tier rule
  in the testing skill says repository adapters are exactly the layer that
  runs against the real dependency — real Postgres, schema applied, FK and
  UNIQUE constraints in place, paired with a `//go:build integration` tag
  when the dependency is heavyweight. The prompt is deliberately neutral
  about test mechanism; with the rule, the agent should reach for real
  Postgres (testcontainers, a docker-compose service, a managed test DB,
  or equivalent) and explicitly refuse the driver-mock path.
assertions:
  - id: real-postgres-proposed
    text: |
      The agent proposes running the test against a real Postgres database
      — concretely, it names testcontainers / `pgtest` / a docker-compose
      service / a CI-provisioned Postgres / `pgxtest` / migrations applied
      to a test schema, OR it pairs the test with a `//go:build integration`
      build tag and describes the real-DB setup the tag gates on. Naming
      the approach counts; producing the wiring code in addition is
      acceptable but not required.
  - id: no-driver-mock
    text: |
      The agent does NOT propose substituting the database with a
      driver-level mock or fake. Specifically, it does not recommend
      `sqlmock` / `go-sqlmock` / `pgxmock`, does not hand-roll a
      `driver.Driver` or `driver.Conn` implementation, and does not
      manually construct synthetic `pgconn.PgError` values to feed through
      a fake `*sql.DB`. Mentioning such an approach only to reject it (as
      part of explaining the fidelity-tier rule) is acceptable; proposing
      it as the test strategy fails this assertion.
  - id: rationale-named
    text: |
      The agent's reasoning explicitly references the fidelity tier or the
      "repository tests run against the real thing" rule (e.g. "real
      Postgres for repository tests", "testing the mock, not the
      translation", "adapter test → real dependency", "highest-fidelity
      option that fits the layer"). Producing the right artifact silently
      does not satisfy this assertion.
---

Add tests for `APIKeyRepository.Create` that cover the three error-translation
paths: the foreign-key violation on `tenant_id`, the foreign-key violation on
`client_id`, and the unique-key violation on `hash`. Edit only files inside
this fixture directory.
