# Safety Practices

## Forbidden Operations

Never do these unless explicitly requested by user:

- Modify source control configuration
- Force push to main/master branches - Destroys history
- Skip pre-commit hooks - They protect code quality
- Amend commits that are already pushed - Rewrites public history
- Hard reset without user confirmation - Loses work
- Clean/delete untracked files without user confirmation

## Amend Safety Protocol

Before amending: Check if pushed and if you're author. Only amend if both false and true.

When: Hook modified files, user requests, typo fix before push
Never: Someone else's commit, already pushed commits

## Pre-Commit Hook Protocol

If hooks modify files: Review changes, re-stage, may amend if not pushed.
If hooks fail: Read error, fix issue, never use `--no-verify`, ask user if unsure.

## Branch Operations

Branch naming conventions:

```
# Feature branches
feature/add-search
feat/fuzzy-matching

# Bug fix branches
fix/cache-race-condition
bugfix/auth-token-refresh

# Personal branches (if applicable)
username/experiment-name
```

Never:
- Work directly on `main`/`master` unless specifically instructed
- Delete branches that are pushed without confirming
- Force push to shared branches
