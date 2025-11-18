# Jujutsu Practices

LLM-specific guidance for using jujutsu when available.

## Understanding Working Copy (Critical)

**The working copy is part of the current commit (@).**

In jujutsu, uncommitted changes are not "staged" - they ARE the current commit until you advance:

```bash
# Make edits to files
jj status
# Shows: Working copy changes:
#        M file.go
# These changes are PART of commit @ right now

jj commit -m "message"
# Now @ is empty and those changes are in @-
```

**Critical implications:**

1. Any files you edit are immediately part of @ (the current commit)
2. If @ already has a description, your edits modify that commit
3. If @ is empty, your edits will become a new commit when you run `jj commit`
4. Moving away from @ with `jj edit` saves your working copy changes into @

**Never assume working copy changes are separate from commits.**
They are the same thing until you advance with `jj commit`.

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

### Empty Commits Are Normal

**Do not panic when you see "(empty)" in jj status or jj log.**

Empty commits are the standard state in jujutsu:

```bash
jj status
# Working copy  (@) : abcd1234 (empty) (no description set)
# This is correct and expected
```

**When @ is empty:**
- You are ready to make new changes
- Next edits will become part of @ (see Understanding Working Copy above)
- When you `jj commit`, those edits become @- and @ advances to new empty commit

**When @ is NOT empty:**
- You have uncommitted changes that are part of @
- You should either `jj commit` to finalize them or continue editing
- Never leave session with non-empty @ unless intentional

Empty commits are jujutsu's way of saying "ready for new work."
They are not errors or problems.

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

### When NOT to Use op restore (Anti-Pattern)

**Do not panic-restore when you make mistakes.**

`jj op restore` is for recovering from catastrophic errors,
not for fixing normal workflow mistakes.

**Use `jj undo` instead:**
```bash
# Made a mistake
jj undo  # Reverts last operation

# Made multiple mistakes
jj undo  # Undo one step at a time
jj undo  # Each undo goes back one operation
```

**Only use `jj op restore` when:**
- Multiple operations need reversal and `jj undo` is too slow
- You identified a specific good state in `jj op log`
- You understand what state you're restoring to

**Anti-pattern - panic restoring:**
```bash
# Bad: Chain of panic restores
jj op restore <id1>  # "Maybe this fixes it"
jj op restore <id2>  # "Wait, that broke more"
jj op restore <id3>  # "Going back further"
# Now you're completely lost
```

**Better pattern - fix forward:**
```bash
# Mistake: Squashed wrong commits
# Don't panic restore
# Instead: Check what happened
jj log
# Fix by editing commits or creating new ones
# Jujutsu makes most operations reversible without op restore
```

**Rule:** If you find yourself using `jj op restore` more than once in a session,
stop and ask the user how to proceed.
You are likely confused and making it worse.

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

**Squashing into non-HEAD commits without checking:**
```bash
# Bad: Blindly squash into past commit
jj new vlmnlpxn
jj file track test.go
jj squash  # Rebases all descendants!

# Good: Check for descendants first
jj log -r 'descendants(vlmnlpxn) & ~vlmnlpxn'
# If descendants exist, use fixup commit instead
jj commit -m "fixup: add test file"
```

**Panic-restoring operations:**
```bash
# Bad: Restoring multiple times
jj op restore <id1>
jj op restore <id2>
jj op restore <id3>

# Good: Use undo or fix forward
jj undo  # One operation at a time
```

**Confusing working copy with staging:**
```bash
# Bad mental model: "These changes are staged"
# Working copy changes are NOT staged
# They ARE the current commit (@)

# Correct understanding:
# Any edits to files are part of @ until you jj commit
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

**Critical: Choosing between -m and -u:**

Use `-u` when:
- Parent commit has correct message
- Adding forgotten files to existing commit
- Fixing minor omissions without changing intent

Use `-m` when:
- Parent message is wrong and needs replacement
- Combining changes requires new unified message

Common mistake: Using `jj squash -m "add missing file"` when parent has good message.
This replaces the parent's message instead of preserving it.
Always check parent message first: `jj log -r @-`

### Squashing Into Non-HEAD Commits (Dangerous)

**Squashing into commits that have descendants will rebase those descendants.**

Before squashing into any commit that is not @-, check for descendants:

```bash
# Check if commit has descendants
jj log -r 'descendants(abcd1234) & ~abcd1234'
# If this shows commits, they will all be rebased
```

**Safe squash pattern:**
```bash
# Only squash @ into @- (immediate parent)
jj squash -u  # Safe - no descendants to rebase
```

**Dangerous squash pattern:**
```bash
# Currently at commit C
# History: A <- B <- C (@)
jj new A  # Jump to commit A
# Add forgotten file
jj squash  # Rebases B and C!
```

**If you must add files to past commits:**

Option 1: Accept that descendants will be rebased
```bash
jj new <past-commit>
# Make changes
jj squash -u
# Descendants are now rebased - verify they still look correct
jj log
```

Option 2: Create fixup commit instead (safer)
```bash
# Stay at current commit
# Create new commit with the fix
jj commit -m "fixup: add missing file to commit X"
# Squash later when safe or leave separate
```

Never squash into non-HEAD commits unless you understand the rebase implications.

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
