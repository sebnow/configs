---
name: jujutsu
description: "LLM-specific guidance for using jujutsu. Complements commit skill with jujutsu-specific workflows: working copy model, commit commands, file tracking, non-interactive operations. Triggers: jj commands, jujutsu workflows, working copy questions, bookmark management."
---

# Jujutsu

## Overview

LLM-specific guidance for using jujutsu effectively.
Follow commit skill for VCS-agnostic principles
(atomic commits, message format, conventions).
This skill covers jujutsu-specific workflows and commands.

## Understanding Working Copy (Critical)

**The working copy is part of the current commit (@).**

In jujutsu, uncommitted changes are not "staged" -
they ARE the current commit until you advance:

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
Never end on a non-empty working commit -
user's changes will auto-commit dangerously.

## Empty Commits Are Normal

**Do not panic when you see "(empty)" in jj status or jj log.**

Empty commits are the standard state in jujutsu:

```bash
jj status
# Working copy  (@) : abcd1234 (empty) (no description set)
# This is correct and expected
```

**When @ is empty:**
- You are ready to make new changes
- Next edits will become part of @
- When you `jj commit`, those edits become @-
  and @ advances to new empty commit

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

Never use `jj new` alone -
it's only for special cases like creating empty commits on different branches.

Use HEREDOC for multi-line messages.

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

**Choosing between `-m` and `-u`:**

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

## References

For advanced operations:
- See [advanced-operations.md](references/advanced-operations.md)
  for squashing safety, bookmark management, and git integration
- See [operation-log.md](references/operation-log.md)
  for operation log safety net, undo vs restore, and recovery patterns

## When Commands Require User Input

If operation needs user decision:
1. State what decision is needed
2. Ask user to specify
3. Use their input in non-interactive command

Don't invoke interactive commands and wait.
