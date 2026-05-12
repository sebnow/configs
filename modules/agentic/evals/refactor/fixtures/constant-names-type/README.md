Temporal-using Go service that handles two workflow update messages.

Each update has a package-level `const` holding its name string and a separate
struct holding its payload. The constant and the struct identify the same
concept (`AppealOutcomeUpdate`, `ReportEscalationUpdate`) but live as
independent declarations: the link between them is a naming convention readers
must recognize, not a relationship the type system enforces.

`workflow.SetUpdateHandlerWithOptions` is the only call site that consumes
both — it takes the constant as the registration name and the struct as the
payload type. Renaming one without the other compiles cleanly and silently
breaks the contract: the registration ends up under the old name while the
payload type carries the new one.

`DefaultTaskQueueName` in the same file is a counter-example — a process-wide
configuration value not owned by any single type. It legitimately stays a
package-level constant.
