Small Go package wiring a repository method to a squirrel-style fluent
builder.

The fixture defines a tiny `QueryBuilder`/`InsertBuilder` API that mirrors
real squirrel: callers can either render SQL with `ToSql()` and run it via
`*sql.DB.ExecContext`, or chain straight through `RunWith(db).ExecContext(ctx)`.
Both terminations are syntactically available; the builder docs are neutral
about which to use. The `Repo.Insert(ctx, Invoice)` stub needs to insert the
invoice fields into the "invoice" table.
