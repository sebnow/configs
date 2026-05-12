Go membership-cancellation service whose `Service.Cancel` performs a single
domain action — cancel a membership, record the refund, write the audit
entry — across three independent database transactions. The three writes
share the same database handle, run sequentially in the same goroutine, and
have no external collaborator between them, yet each begins and commits its
own transaction. A crash, network blip, or transient SQL error between any
two steps leaves the system in a partial state that has to be reconciled
by hand: cancellations with no refund, refunds with no audit trail, audit
entries that disagree with the membership state.

The fragmentation is structural, not domain-driven. Nothing in the flow
requires that the membership state commit before the refund row is written
or before the audit entry is appended. Collapsing the three writes into a
single transactional boundary — one `BeginTx`, three `ExecContext` calls,
one `Commit` — makes the cancel-membership outcome atomic and removes the
partial-state recovery work entirely.

`notify.go` is a deliberate counter-example. `Service.NotifyCanceled`
writes to a local outbox table and then calls a remote billing API. Those
two operations cannot share a transaction because the billing API is an
external system the database cannot enroll in `BeginTx` / `Commit`. The
right shape there is a saga / outbox pattern with retries and idempotency
keys — the fragmentation is domain-driven and should remain.
