# Commit Message Structure

## Format

```
<subject line>

<body (optional)>

<trailers (optional)>
```

## Subject Line

Length: Aim for 50 characters, max 72

Mood: Use imperative mood ("Add feature" not "Adds feature" or "Added feature")

Focus: Describe why, not what (the diff shows what changed)

Examples:

Bad (describes what):
```
Update user.go, add validation function, change error handling
```

Good (describes why):
```
Prevent invalid email addresses from being stored
```

Bad (too vague):
```
Fix bug
```

Good (specific):
```
Fix race condition in cache invalidation
```

## Conventional Commits Format

When project uses conventional commits:

```
<type>(<scope>): <description>
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `chore`
Scope: Optional component/module name

Examples: `feat(search): add fuzzy matching`, `fix(auth): prevent token refresh race`, `docs: add installation guide`

## Body (Optional)

Include when: Why isn't obvious, trade-offs exist, context helps maintainers, complex changes
Skip when: Obvious, simple refactoring/fix

Guidelines: Wrap at 72 chars, blank line after subject, explain why not what, note trade-offs
Never: Promotional text, emoji (unless project uses), unnecessary metadata

## Template

```
[type(scope): ] <imperative description of why>

[Optional: Context if not obvious]
```

Include `type(scope):` prefix only if project uses conventional commits.

## Examples

Good (atomic, clear why):
```
feat(auth): add token refresh mechanism

Prevents users from being logged out during active sessions.
Refresh happens 5 minutes before expiration.

---

test(auth): add token refresh integration tests

---

fix(auth): handle edge case when refresh fails

Previously crashed on network timeout. Now retries with exponential backoff.
```

Bad (non-atomic, vague):
```
Update auth stuff

Added refresh tokens, fixed some bugs, updated tests, refactored validation
```

## Generated-Code Commit

A regeneration ships in its own commit. The subject is always
`chore: regenerate code` (an optional scope is fine). The commit
contains only generator output — no hand-written file, not even the
source that triggered the regeneration.

Good (generated diff, isolated):
```
chore: regenerate code

Regenerate pb/user.pb.go after the UserResponse field addition in
proto/user.proto (committed separately).

---

feat(api): add GetUser endpoint

Expose user lookup over HTTP. The proto change for UserResponse and
its regenerated Go binding ship in adjacent commits.
```

Bad (generated diff mixed with intent):
```
feat(api): add GetUser endpoint

Add handler, update proto, regenerate pb/user.pb.go and mocks.
```

## Breaking-Change Commit

When a change removes or renames a public symbol, the subject must
carry the Conventional Commits `!` marker and the body must include
a `BREAKING CHANGE:` footer naming the migration path. Both are
required.

Good (subject marks the break; body names the migration):
```
refactor(db)!: rename ConnectDB to OpenDB

Align the connection helper with the `database/sql` standard-library
naming convention (sql.Open, sql.OpenDB).

BREAKING CHANGE: ConnectDB has been removed. Callers must replace
`db.ConnectDB(dsn)` with `db.OpenDB(dsn)`; the signature and return
values are unchanged.
```

Good (feat with breaking proto-field rename):
```
feat(api)!: rename UserResponse.name to displayName

Reflect the user-facing label across REST and gRPC clients.

BREAKING CHANGE: the `name` field on `UserResponse` is removed.
Consumers must read `displayName` instead. Old clients receive an
empty string until they migrate.
```

Bad (breakage hidden in prose; no `!`, no footer):
```
refactor(db): rename ConnectDB to OpenDB

This renames the connection helper. Callers should switch to OpenDB.
```
