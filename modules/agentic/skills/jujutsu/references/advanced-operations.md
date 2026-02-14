# Advanced Operations

## Squashing Into Non-HEAD Commits (Dangerous)

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

Never squash into non-HEAD commits
unless you understand the rebase implications.

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

## Integration with Git Workflows

**Fetch and push:**
```bash
jj git fetch                          # Fetch from git remotes
jj git push --bookmark feature-name   # Push to git remotes
```

**Working with git remotes:**
Jujutsu handles git remotes transparently.
No special setup needed for GitHub/GitLab workflows.
