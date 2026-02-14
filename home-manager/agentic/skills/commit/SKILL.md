---
name: commit
description: "MUST use before any commit operation. Detects VCS type (prefers jujutsu over git), enforces atomic commits, validates messages. Required before: 'git commit', 'jj commit', 'commit these changes', 'let me commit', 'ready to commit', 'git add', creating commits, branch operations, pull requests."
---

# Commit

## Overview

Good commits enable clear history, easy debugging, and confident collaboration.
Bad commits create confusion, make debugging harder, and complicate code review.

Core principle: Every commit tells a clear story and is independently reviewable.

If the project uses jujutsu (`.jj` directory exists),
you MUST load and follow the jujutsu skill before running any commands.
Do not rely on your own knowledge of jujutsu.

## Detecting Project Conventions (Required)

Before your first commit, you must:

1. Examine recent commit history
2. Use task tracking to document findings:
   ```
   - [completed] Convention check: Uses conventional commits (feat/fix/docs)
   - [completed] Convention check: Lowercase subject, no period
   - [completed] Convention check: Imperative mood
   ```
3. State explicitly: "This project [uses/doesn't use] conventional commits"

If you skip this step, you will violate project conventions.

### Convention Detection Checklist

- [ ] Conventional Commits: `type(scope): description` pattern? (feat/fix/docs/refactor/test/chore)
- [ ] Capitalization: Uppercase or lowercase subjects?
- [ ] Punctuation: Periods at end?
- [ ] Mood: Imperative ("add") vs past tense ("added")?
- [ ] Length: ~50 chars or longer?
- [ ] Emoji: Used or not?

Follow project convention if it exists, otherwise use general guidelines below.

## Atomic Commits

Definition: One logical change per commit.

### What Makes a Commit Atomic?

Good atomic commits:

- Add a single feature with its tests
- Fix one bug with its test
- Refactor one component
- Update related configuration files together
- Include necessary prerequisites (e.g., add helper function + use it)

Bad atomic commits:

- Fix three unrelated bugs
- Add feature + unrelated refactoring
- Mix formatting changes with logic changes
- Change multiple unrelated files "while you're at it"

### The Atomic Commit Test

Ask yourself:

1. Can I describe this commit in one sentence without using "and"?
2. If this commit were reverted, would it cleanly undo one logical change?
3. Could someone review this commit in isolation and understand it?

If any answer is "no", split the commit.

### When to Split Commits

Split: Multiple bugs, feature + refactoring, independent components, substantial preparation work

Don't split: Function definition + usage, test + implementation, config + feature that needs it

## Commit Message Structure

See [commit-messages.md](references/commit-messages.md)
for detailed format, examples, and templates.

Key requirements:
- Subject: 50 chars (max 72), imperative mood, describe why not what
- Body: Optional, explain context when not obvious
- Follow project conventions (conventional commits if used)

## Pre-Commit Review (Required)

Before creating a commit, review and verify:

1. **Review diff** for debug statements, commented-out code,
   TODO/FIXME markers, unintended files
2. **Verify atomic scope** - one logical change only
3. **Stage selectively** - specific files, not `git add .` or blind inclusion
4. **Validate commit message** against project conventions:
   - [ ] Follows project convention (conventional commits if used)
   - [ ] Subject line 50 chars or less
   - [ ] Uses imperative mood ("Add" not "Added/Adds")
   - [ ] Describes why, not what
   - [ ] Body wrapped at 72 chars (if present)

Only after all checks pass may you create the commit.

## Post-Commit Verification

After committing, review message and diff. Verify:
- [ ] Message follows conventions
- [ ] Only intended changes included
- [ ] No debug statements committed

If check fails:
Amend if not pushed (per safety practices in agents.md),
else create fixup commit.

## Anti-Patterns

If you are about to say or think any of these phrases, STOP IMMEDIATELY:

**Anti-Patterns:**
- "commit all changes" / "git add ." / "add all files" -> Stage selectively
- "fixes A, B, and C" / "adds X and fixes Y" -> Split into atomic commits
- "skip checking conventions" -> Check conventions first
- "amend this commit" -> Check if pushed first
- "force push" -> Never without explicit user request
- "Quick commit" / "WIP commit" -> Not without user request

Jujutsu-specific anti-patterns are covered in the jujutsu skill.

**When detected:**
1. Stop immediately
2. State which requirement you skipped
3. Complete the requirement before proceeding
4. Resume with correct approach

## Examples

See [commit-messages.md](references/commit-messages.md)
for good and bad commit message examples.
