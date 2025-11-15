# Jujutsu Practices

LLM-specific guidance for using jujutsu when available.

## Commit Workflow (Critical)

**Always use `jj commit -m` to create commits.**

Standard workflow:
```bash
# 1. Make changes and track new files
jj file track <new-files>

# 2. Verify changes
jj status

# 3. Commit and advance to new empty working commit
jj commit -m "$(cat <<'EOF'
Commit message
EOF
)"

# 4. Verify @ is empty
jj status  # Should show "(empty)"
```

**Critical:** After every commit, @ must be empty.
Never end on a non-empty working commit - user's changes will auto-commit dangerously.

## Amending Commit Messages

**Use `jj describe` only to amend existing commit messages.**

```bash
# Fix typo in current commit message
jj describe -m "Corrected message"

# Fix message in parent commit
jj describe -r @- -m "Corrected message"
```

Never use `jj describe` + `jj new` to create commits.
That pattern is wrong - always use `jj commit -m`.

## Detecting Jujutsu

Before defaulting to git, check if jujutsu is in use:

```bash
# Check for .jj directory
ls -la .jj

# Or check recent command history
jj log --limit 5
```

If jujutsu is available and has been used, prefer it over git.

## Prefer Jujutsu Over Git

When both available, use jujutsu:

- User has chosen jujutsu for a reason (better UX, safety features)
- Most git commands have jj equivalents
- Mixing creates inconsistent history

Use git only when jujutsu lacks needed functionality.

## File Tracking

**Critical: Users often disable auto-tracking.**

After creating new files, always track them explicitly:

```bash
jj file track <path>
```

Before committing, check status:
```bash
jj status  # Shows untracked files with "?"
```

Files marked with "?" are not included in commits.
You must track them first.

**Pre-commit protocol:**
1. Check `jj status`
2. Track any new files with `jj file track`
3. Verify files show "A" (added) not "?" (untracked)
4. Then commit

Never commit without checking status first.

## Non-Interactive Commands

Always use non-interactive flags.

**Creating new commits:**
```bash
# Always use jj commit -m
jj commit -m "$(cat <<'EOF'
Subject line

Body text here.
EOF
)"
```

Never use `jj new` alone - it's only for special cases like creating empty commits on different branches.

## Commit Message Best Practices

Follow same guidelines as git:

```bash
jj commit -m "$(cat <<'EOF'
feat(auth): add token refresh

Prevents session timeout during active use.
Refresh occurs 5 minutes before expiration.
EOF
)"
```

Use HEREDOC for multi-line messages.
Same atomic commit principles apply.

## Safety Practices

**Operation log provides safety net:**
```bash
jj op log               # View all operations
jj undo                 # Undo last operation
```

Unlike git reflog, operation log is comprehensive and easier to use.

**Immutability protects shared work:**
- Main branches and pushed commits cannot be rewritten
- Safer than git's mutable history

**When mistakes happen:**
1. Use `jj undo` for immediate reversal
2. Use `jj op restore <id>` for earlier states
3. No need for complex git reset/reflog operations

## Bookmark Management

**Creating bookmarks:**
```bash
jj bookmark set feature-name           # At current change
jj bookmark set feature-name -r <rev>  # At specific change
```

**Pushing bookmarks:**
```bash
# Explicit bookmark required (prevents accidental pushes)
jj git push --bookmark feature-name
jj git push --bookmark feature-name --allow-new  # For new bookmarks
```

Never push without specifying bookmark.
Jujutsu won't guess which bookmark to push (unlike git's current branch).

## Common Anti-Patterns

**Using `jj describe` + `jj new` to create commits (wrong pattern):**
```bash
# Bad: Wrong pattern for creating commits
jj describe -m "message"
jj new

# Good: Always use jj commit
jj commit -m "message"
```

**Ending on non-empty working commit (dangerous):**
```bash
# Bad: @ is non-empty after this
jj describe -m "message"
# User's next file changes auto-commit without warning!

# Good: @ is empty after jj commit
jj commit -m "message"
# User's changes are safe in new working commit
```

**Using `jj new` when unnecessary:**
```bash
# Bad: Creates empty commits
jj new  # Almost never needed

# Good: Just use jj commit
jj commit -m "message"
```

`jj new` is only for special cases (creating empty commits on branches).
Never use it in normal commit workflows.

**Defaulting to git when jj available:**
```bash
# Bad: User has jujutsu installed
git commit -m "message"

# Good: Use their preferred tool
jj commit -m "message"
```

**Interactive operations:**
```bash
# Bad: Waits for user input
jj split
jj squash
jj squash -r <change>

# Good: Non-interactive with -m or -u flags
jj squash -m "New message"          # Squash @ into parent with new message
jj squash -u                        # Squash @ into parent, keep message
jj squash -r <change> -m "message"  # Squash specific change with message
jj squash -r <change> -u            # Squash specific change, keep message
jj squash --into <dest> -m "msg"    # Squash @ into dest with message
jj squash --into <dest> -u          # Squash @ into dest, keep message
```

Always use `-m` (new message) or `-u` (keep existing message).
Without these flags, commands open interactive editor.

## Integration with Git Workflows

**Fetch and push:**
```bash
jj git fetch                          # Fetch from git remotes
jj git push --bookmark feature-name   # Push to git remotes
```

**Working with git remotes:**
Jujutsu handles git remotes transparently.
No special setup needed for GitHub/GitLab workflows.

## Creating vs Amending

**Creating commits:** Always use `jj commit -m`
**Amending messages:** Only use `jj describe -m`

Never mix these patterns.
Never use `jj describe` + `jj new` to create commits.

## When Commands Require User Input

If operation needs user decision:
1. State what decision is needed
2. Ask user to specify
3. Use their input in non-interactive command

Don't invoke interactive commands and wait.
