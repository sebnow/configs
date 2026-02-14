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
