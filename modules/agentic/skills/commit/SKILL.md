---
name: commit
description: "MUST use before any commit operation. Detects VCS type (prefers jujutsu over git), enforces atomic commits, validates messages. Required before: 'git commit', 'jj commit', 'commit these changes', 'let me commit', 'ready to commit', 'git add', creating commits, branch operations, pull requests."
---

# Commit

## Overview

Good commits enable clear history, easy debugging, and confident collaboration.
Bad commits create confusion, make debugging harder, and complicate code review.

Core principle: Every commit tells a clear story and is independently reviewable.

If the project uses jujutsu,
you MUST load and follow the jujutsu skill before running any commands.
Do not rely on your own knowledge of jujutsu.

## Detecting Project Conventions (Required)

Before your first commit,
analyze recent commits for project conventions.
If recent commits are already visible in the conversation,
use those — do not fetch history redundantly.

State explicitly: "This project [uses/doesn't use] conventional commits."

If you skip this step, you will violate project conventions.

### Convention Detection Checklist

- [ ] Conventional Commits: `type(scope): description` pattern? (feat/fix/docs/refactor/test/chore)
- [ ] Capitalization: Uppercase or lowercase subjects?
- [ ] Punctuation: Periods at end?
- [ ] Mood: Imperative ("add") vs past tense ("added")?
- [ ] Length: ~50 chars or longer?
- [ ] Emoji: Used or not?

Follow project convention if it exists, otherwise use general guidelines below.

## Commit Cadence

Commit after each logical change, not after finishing all work.

If you have multiple uncommitted logical changes, stop.
Split them into separate commits before proceeding.

## Atomic Commits

Definition: One logical change per commit.
The commit is a unit of reasoning, not a unit of effort.

### What Makes a Commit Atomic?

Good atomic commits:

- Add a single feature with its tests
- Fix one bug with its test
- Refactor one component (no behaviour change)
- Update related configuration files together
- Include necessary prerequisites (e.g., add helper function + use it)

Bad atomic commits:

- Fix three unrelated bugs
- Add feature + unrelated refactoring
- Mix formatting changes with logic changes
- Change multiple unrelated files "while you're at it"
- Add two independent parsers or modules in one commit
- Bundle a review's worth of fixes into one commit

### Separate Mechanical from Functional Changes

Mechanical changes (renames, moves, reformatting, dead code removal)
must be in separate commits from functional changes (new behaviour, bug fixes).

A reviewer can verify a mechanical commit in seconds
by confirming no behaviour changed.
Mixing them forces line-by-line review of the entire diff.

### The Atomic Commit Test

Before every commit, verify:

1. Can I describe this commit in one sentence without using "and"?
2. If this commit were reverted, would it cleanly undo one logical change?
3. Could someone review this commit in isolation and understand it?

If any answer is "no", split the commit.

### When to Split Commits

Split: Multiple bugs, feature + refactoring, independent components,
substantial preparation work, mechanical + functional changes

Don't split: Function definition + usage, test + implementation,
config + feature that needs it

## Commit Message Structure

See [commit-messages.md](references/commit-messages.md)
for detailed format, examples, and templates.

Key requirements:

- Subject: 50 chars (max 72), imperative mood
- Describe why, not what — the diff shows what changed;
  the message must supply the reasoning the diff cannot convey
- Body: Optional, explain context when not obvious
- Follow project conventions (conventional commits if used)

## Pre-Commit Review (Required)

Before running any commit command, you must complete all steps below.

### Step 1: Enumerate Changes

Review the diff and list every distinct modification
(renamed function, added function, changed import, added parameter, etc.).

### Step 2: Verify Atomicity

Apply the atomic commit test to the enumerated list:

- Could any change be committed independently?
- Are there mechanical changes mixed with functional changes?
- Would reverting this commit undo more than one decision?

If yes to any: you must split into separate commits.
Do not ask — unstage, re-stage selectively,
and commit each change separately.

### Step 3: Review Content

Check for debug statements, commented-out code,
TODO/FIXME markers, unintended files.

### Step 4: Stage Selectively

Stage specific files, not `git add .` or blind inclusion.

### Step 5: Validate Message

- [ ] Follows project convention (conventional commits if used)
- [ ] Subject line 50 chars or less
- [ ] Uses imperative mood ("Add" not "Added/Adds")
- [ ] Describes why, not what
- [ ] Body wrapped at 72 chars (if present)

Only after all steps pass may you create the commit.

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
- "I'll commit everything at the end" -> Commit after each logical change
- "These are related so I'll commit them together" -> Apply the atomic commit test
- "add X and Y parsers/modules" -> Each independent module is a separate commit
- "review fixes" / "various improvements" -> Each fix is a separate commit
- "refactor and fix" -> Mechanical and functional changes are separate commits

Jujutsu-specific anti-patterns are covered in the jujutsu skill.

**When detected:**

1. Stop immediately
2. State which requirement you skipped
3. Complete the requirement before proceeding
4. Resume with correct approach

## Examples

See [commit-messages.md](references/commit-messages.md)
for good and bad commit message examples.
