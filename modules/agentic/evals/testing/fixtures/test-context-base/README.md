Auth helpers for a moderation service.

`RequireAPIKey` extracts an `APIKey` from a `context.Context`, returning
`ErrMissingAPIKey` when none is present. The package only has a single
round-trip test today, scaffolded by a teammate — `auth_test.go` uses
`context.Background()` because the original author defaulted to it.

The task is to extend `auth_test.go` to cover both `RequireAPIKey` paths
(present and missing). Apply the test conventions of this codebase when
choosing the base context, and adjust the existing scaffolding if it does
not match those conventions. There is no Postgres or other infrastructure
in this fixture — only the standard library.
