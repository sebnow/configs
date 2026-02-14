# Operation Log

## Safety Net

**Operation log provides safety net:**
```bash
jj op log               # View all operations
jj undo                 # Undo last operation
```

Unlike git reflog, operation log is comprehensive and easier to use.

**Immutability protects shared work:**
- Main branches and pushed commits cannot be rewritten
- Safer than git's mutable history

## `jj undo` vs `jj op restore`

**Use `jj undo` for immediate reversal:**
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

## Panic-Restoring Anti-Pattern

**Do not panic-restore when you make mistakes.**

`jj op restore` is for recovering from catastrophic errors,
not for fixing normal workflow mistakes.

**Anti-pattern - panic restoring:**
```bash
# Bad: Chain of panic restores
jj op restore <id1>  # "Maybe this fixes it"
jj op restore <id2>  # "Wait, that broke more"
jj op restore <id3>  # "Going back further"
# Now you're completely lost
```

## Fix-Forward Pattern

**Better pattern - fix forward:**
```bash
# Mistake: Squashed wrong commits
# Don't panic restore
# Instead: Check what happened
jj log
# Fix by editing commits or creating new ones
# Jujutsu makes most operations reversible without op restore
```

## Stop and Ask Rule

**Rule:** If you find yourself using `jj op restore` more than once in a session,
stop and ask the user how to proceed.
You are likely confused and making it worse.
