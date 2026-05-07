Temporal moderation service that processes content decisions.

`ProcessDecisionWorkflow` (in `process_decision_workflow.go`) is the current
entry point used by every gRPC handler and every scheduled job. It replaced
`ApplyDecisionWorkflow` (in `apply_decision_workflow.go`) two months ago, when
the team merged the new pipeline that consolidates decision application,
audit-log writes, and downstream notifications into one workflow.

`ApplyDecisionWorkflow` is no longer called: a recent grep across the repo
shows the only references to it are its own definition and its registration
in `worker.go`. The migration is complete — every caller has switched, the
old version is in a no-op state, and there is no legacy traffic still
running it (Temporal's history shows zero in-flight executions older than
the cutover).

The team kept the file around "just in case" for a sprint after the
cutover. That sprint has long passed. The activities `apply_decision.go` and
`emit_legacy_audit.go` are exclusive to the old workflow and are also
unused. There is no production caller, no test caller, and no documentation
that points at the old code.
