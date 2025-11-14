# Jujutsu Practices

LLM-specific guidance for using jujutsu when available.

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

Files marked with "?" are NOT included in commits.
You MUST track them first.

**Pre-commit protocol:**
1. Check `jj status`
2. Track any new files with `jj file track`
3. Verify files show "A" (added) not "?" (untracked)
4. Then commit

Never commit without checking status first.

## Non-Interactive Commands

Always use non-interactive flags.

**Creating new changes:**
```bash
# Good: Creates message AND advances to new change
jj commit -m "$(cat <<'EOF'
Subject line

Body text here.
EOF
)"

# Wrong: Only sets message, doesn't advance
jj describe -m "message"
```

**Modifying existing change description:**
```bash
# Good: Update description of current change
jj describe -m "Updated message"

# Also good: Interactive is fine for editing existing
jj describe
```

Critical distinction: `jj commit` creates message AND advances to new working change.
`jj describe` only sets/updates message on current change.

**Creating changes:**
```bash
jj new main             # Non-interactive, safe
jj new -m "WIP feature" # With message immediately
```

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

**Clobbering commits by not advancing:**
```bash
# Bad: Continues modifying same change, clobbers previous work
jj describe -m "Fix bug"
# Make more changes...
jj describe -m "Fix another bug"  # CLOBBERED previous commit!

# Good: Each change gets its own commit
jj commit -m "Fix bug"
# Make more changes (auto-amends new working change)
jj commit -m "Fix another bug"    # Separate commit
```

Critical: When creating NEW changes, use `jj commit` to advance.
Working directory is always @ - if you don't advance, you clobber previous work.

**Using describe when you need to advance:**
```bash
# Bad: Only sets message, stays on same change
jj describe -m "message"
# Next changes still modify same commit!

# Good: Sets message AND advances to new working change
jj commit -m "message"
# Next changes go into new commit
```

`jj describe` is fine for modifying existing change descriptions.
Use `jj commit` when you need to create a new change and move forward.

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

## Advancing vs Modifying

Understand when to advance to new change vs modify existing:

**Advance with `jj commit`:**
- Completed a logical change (feature, bug fix, refactor)
- Need to start new work
- Creating atomic commits
- Following multi-commit workflow

**Modify with `jj describe`:**
- Fixing typo in commit message
- Updating description of current work
- Amending message before advancing

When in doubt, use `jj commit` to advance.
Prevents clobbering previous commits.

## When Commands Require User Input

If operation needs user decision:
1. State what decision is needed
2. Ask user to specify
3. Use their input in non-interactive command

Don't invoke interactive commands and wait.
