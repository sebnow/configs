---
id: 23-consolidate-to-one-atomic-unit
title: Service.Cancel fragments a single domain outcome across three transactions
fixture: fixtures/fragmented-cancel-membership
expect_trigger: true
target_lens: shallow-modules
category: pressure
rationale: |
  `Service.Cancel` performs three sequential writes — flip the membership
  state, insert the refund row, append the audit-log entry — and wraps
  each one in its own `BeginTx` / `Commit`. The three writes share the
  same database handle, run in the same goroutine with no external
  collaborator between them, and represent a single domain outcome
  ("cancel a membership") that the business expects to commit-or-fail as
  one. A crash or transient SQL error between any two steps leaves the
  system in a partial state — cancellations without refunds, refunds
  without audit, audit entries disagreeing with membership state — that
  operators have to clean up by hand. The catalog move is "Consolidate
  to One Atomic Unit" — collapse the three transactions into a single
  transactional boundary (one `BeginTx`, all three `ExecContext`s, one
  `Commit`) so the outcome is atomic and the recovery work disappears.
  `Service.NotifyCanceled` in `notify.go` is a deliberate counter-example:
  it writes to a local outbox and then calls a remote billing API, which
  cannot share a transaction because the API is an external system the
  database cannot enrol in `BeginTx` / `Commit`. Without the catalog the
  agent may flag this as "use a single transaction", "make it atomic",
  "wrap in tx", or "extract a helper" without naming the move that
  resolves it.
assertions:
  - id: numbered-candidates
    text: |
      The agent presents at least one numbered candidate (e.g., "Candidate 1:"
      or "1.") and pauses for user direction before proposing interface
      signatures or jumping into implementation.
  - id: atomic-vs-saga
    text: |
      The agent observes that the three writes inside `Service.Cancel`
      (membership state update, refund insert, audit-log insert) share
      the same database handle and represent a single domain outcome
      that the business expects to commit-or-fail together, and that
      fragmenting them into separate transactions produces partial
      states on crash or transient failure. It distinguishes this from
      `Service.NotifyCanceled` in `notify.go`, which mixes a local
      database write with a remote billing API call that cannot share
      a transaction and should remain a saga / outbox pattern.
  - id: named-move-consolidate-to-one-atomic-unit
    text: |
      The agent names the move it is proposing using the verbatim
      catalog phrase "Consolidate to One Atomic Unit". A paraphrase
      such as "use a single transaction", "make it atomic", "wrap in
      one tx", "merge the transactions", or "introduce one
      transactional boundary" does NOT satisfy this assertion — the
      agent must reproduce the exact catalog phrase. The proposal
      must also indicate that the consolidation collapses the three
      writes into one transactional boundary (one Begin, all writes,
      one Commit), not merely that they should be grouped or
      sequenced differently.
---

Please review this codebase and identify any refactoring candidates worth exploring.
