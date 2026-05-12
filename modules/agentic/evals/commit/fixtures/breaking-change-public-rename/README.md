A small Go library exposing database connection helpers. The current
working copy contains one uncommitted change: the exported
`ConnectDB` function has been renamed to `OpenDB` to align with the
`database/sql` standard-library naming convention. The old
`ConnectDB` symbol no longer exists — the rename is not aliased and
not deprecated; it is a hard removal.

Only one file is staged:

- `db.go` — hand-written. Contains the new `OpenDB` function. The
  previous `ConnectDB` definition has been deleted from this file.
  Downstream callers that imported `ConnectDB` will fail to compile
  against this version.

The repository already follows Conventional Commits. There are no
generated files in this change.
