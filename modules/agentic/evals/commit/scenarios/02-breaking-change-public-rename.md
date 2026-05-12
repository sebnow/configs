---
id: 02-breaking-change-public-rename
title: Public symbol rename uses Conventional Commits ! and BREAKING CHANGE body
fixture: fixtures/breaking-change-public-rename
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  The fixture removes an exported Go symbol (`ConnectDB`) and replaces
  it with a differently-named one (`OpenDB`). Downstream consumers
  that imported `ConnectDB` will not compile against this revision.
  Conventional Commits requires such changes carry the `!` marker on
  the subject line and a `BREAKING CHANGE:` footer/body line stating
  the migration path so downstream maintainers can act. Without an
  explicit rule the agent typically writes `refactor(db): rename
  ConnectDB to OpenDB` — atomic and well-scoped, but silent about the
  breakage and missing the migration hint reviewers need.
assertions:
  - id: subject-uses-bang
    text: |
      The proposed commit subject uses the Conventional Commits `!`
      marker — for example `feat(db)!: rename ConnectDB to OpenDB`,
      `refactor(db)!: rename ConnectDB to OpenDB`, or any
      `<type>(<scope>)!:` / `<type>!:` form. A subject without `!`
      (e.g., `refactor(db): rename ConnectDB to OpenDB`) is a FAIL,
      even if the body mentions the breakage.
  - id: body-has-breaking-change-line
    text: |
      The commit body contains a line that begins with the literal
      token `BREAKING CHANGE:` (uppercase, with the colon). A body
      that describes the breakage in prose without the
      `BREAKING CHANGE:` token is a FAIL. `BREAKING-CHANGE:` (with a
      hyphen) is acceptable per the Conventional Commits spec.
  - id: breaking-change-states-migration
    text: |
      The text following `BREAKING CHANGE:` names the migration the
      caller must perform — at minimum, it mentions that callers
      should switch from `ConnectDB` to `OpenDB` (or equivalent
      wording such as "replace ConnectDB with OpenDB", "use OpenDB
      instead", "ConnectDB is removed; use OpenDB"). A
      `BREAKING CHANGE:` line that only restates the rename without
      naming the replacement call is a FAIL.
---

The repository has one file staged for commit:

- `db.go` — hand-written. The exported `ConnectDB` function has been
  renamed to `OpenDB`. The old symbol is removed entirely (no alias,
  no deprecation shim). Downstream callers that imported `ConnectDB`
  will fail to compile against this revision.

Plan the commit for this change. Provide the commit message (subject
line, plus body if any). Do not run any git or jj commands — produce
the message as text only.
