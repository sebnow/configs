---
id: 02-method-chaining-builder
title: Builder chain runs end-to-end through RunWith.ExecContext
fixture: fixtures/builder-chain-squirrel
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  The squirrel-style builder in the fixture supports two terminations:
  `ToSql()` followed by `db.ExecContext(sql, args...)`, or
  `RunWith(db).ExecContext(ctx)` chained off the builder itself. Both are
  syntactically valid. Without the method-chaining idiom encoded in the
  coding skill, agents tend to materialise intermediate values
  (`query := ...`, `sql, args, err := query.ToSql()`, then a separate
  `r.db.ExecContext`) because that style mirrors classic database/sql
  examples. With the idiom, the implementation collapses to a single
  expression statement that ends in `RunWith(r.db).ExecContext(ctx)`.
assertions:
  - id: chain-terminates-in-execcontext
    text: |
      The agent's implementation of `Repo.Insert` produces a single chained
      expression that ends in `RunWith(r.db).ExecContext(ctx)` (or
      `RunWith(...).ExecContext(...)` with the same arguments). The
      `Insert(...).Columns(...).Values(...)` builder calls flow directly
      into `RunWith` without being assigned to an intermediate variable.
  - id: tosql-not-used
    text: |
      The agent does not call `ToSql()` on the builder. The implementation
      bypasses SQL/args rendering entirely and relies on the builder's
      `RunWith(...).ExecContext(...)` chain to execute the statement.
  - id: signature-preserved
    text: |
      The agent does not change the signature of `Repo.Insert`, the
      `Invoice` struct, or any of the builder methods. The fix lives
      inside the body of `Insert` only.
---

Implement the body of `Repo.Insert` in `repo.go` so it inserts the invoice's
fields into the `invoice` table using the query builder. Columns, in order:
`id`, `tenant_id`, `customer_id`, `amount_cents`. Return the error from the
database call unwrapped.
