---
name: source-control-hygiene
description: "Required when creating commits, managing branches, or performing source control operations. Enforces atomic commits, proper commit messages, conventional commits (when used), and safety practices. Auto-invokes on: commit creation, branch operations, git/jj commands, pull requests."
---

# Source Control Hygiene

## Overview

Good source control hygiene enables clear history, easy debugging, and confident collaboration.
Bad commits create confusion, make debugging harder, and complicate code review.

Core principle: Every commit should tell a clear story and be independently reviewable.

## When This Skill Activates

This skill is active during:

1. Before first commit in session - Check conventions
2. Before each commit - Follow pre-commit protocol
3. After making changes - Plan atomic commits
4. When considering branch operations - Check safety practices

Activation trigger phrases:
- "Create a commit"
- "I'll commit these changes"
- "Ready to commit"
- "Let me commit"

When you think any of these phrases, this skill must guide your actions.

## Detecting Project Conventions (Required)

Before your first commit, you must:

1. Examine recent commit history
2. Use TodoWrite to document findings:
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

## Multi-Commit Planning

When implementing features requiring multiple commits:

1. Plan sequence before starting: List changes, identify dependencies, ensure each is reviewable
2. Use TodoWrite to track planned commits
3. Complete commits in order (finish, commit, move to next)

Don't make all changes then split retroactively, create out of order, or mix changes.

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

### Format

```
<subject line>

<body (optional)>

<trailers (optional)>
```

### Subject Line

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

### Conventional Commits Format

When project uses conventional commits:

```
<type>(<scope>): <description>
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `perf`, `test`, `chore`
Scope: Optional component/module name

Examples: `feat(search): add fuzzy matching`, `fix(auth): prevent token refresh race`, `docs: add installation guide`

### Body (Optional)

Include when: Why isn't obvious, trade-offs exist, context helps maintainers, complex changes
Skip when: Obvious, simple refactoring/fix

Guidelines: Wrap at 72 chars, blank line after subject, explain why not what, note trade-offs
Never: Promotional text, emoji (unless project uses), unnecessary metadata

## Pre-Commit Protocol (Required)

Before creating a commit, you must complete all steps below.

Forbidden before committing:

- Committing without running tests
- Including files not reviewed in diff
- Committing debug statements or TODO markers
- Creating commit message before completing changes

You must:

1. Review complete diff of changes to be committed
2. Run tests and verify they pass
3. Verify atomic scope (one logical change only)
4. Validate commit message against template

Only after all checks pass may you create the commit.

### 1. Review Changes

Review the diff of what will be committed.

Check:

- [ ] No debug statements left behind
- [ ] No commented-out code
- [ ] No TODO/FIXME unless intentional
- [ ] No unintended files (build artifacts, local configs)
- [ ] Changes match the intended commit scope

### 2. Run Tests (Required)

You must actually execute tests, not just claim you will.

Steps:
1. Identify which tests cover your changes
2. Execute test command (pytest, cargo test, npm test, etc.)
3. Read test output completely
4. Verify all tests pass

Forbidden:
- "The tests should pass" -> Run them
- "I'll assume tests pass" -> Run them
- "Tests would pass" -> Run them
- Committing without test output

Required output:
- State: "Tests passed: [test command output summary]"
- Or: "Tests failed: [fix before committing]"

Commit checklist:

- [ ] Tests pass
- [ ] No new test failures introduced
- [ ] New code is covered by tests (if applicable)

### 3. Check Commit Scope

Review what files are included in the commit.

Verify:

- [ ] Only files related to this atomic change are included
- [ ] No unrelated files sneaked in
- [ ] Split into multiple commits if needed

### 4. Stage Selectively

Do: Select specific files or changes to include in each commit

Don't: Blindly include all changes without reviewing them

### 5. Validate Commit Message

Before creating commit, verify your message:

- [ ] Follows project convention (conventional commits if used)
- [ ] Subject line 50 chars or less
- [ ] Uses imperative mood ("Add" not "Added/Adds")
- [ ] Describes why, not what
- [ ] No "Generated by" promotional text
- [ ] Body wrapped at 72 chars (if present)

If any check fails, revise message before committing.

## Post-Commit Verification

After committing, review message and diff. Verify:
- [ ] Message follows template
- [ ] Only intended changes included
- [ ] No debug statements committed

If check fails: Amend if not pushed (per safety protocol), else create fixup commit.

## Safety Practices

### Forbidden Operations

Never do these unless explicitly requested by user:

- Modify source control configuration
- Force push to main/master branches - Destroys history
- Skip pre-commit hooks - They protect code quality
- Amend commits that are already pushed - Rewrites public history
- Hard reset without user confirmation - Loses work
- Clean/delete untracked files without user confirmation

### Amend Safety Protocol

Before amending: Check if pushed and if you're author. Only amend if both false and true.

When: Hook modified files, user requests, typo fix before push
Never: Someone else's commit, already pushed commits

### Pre-Commit Hook Protocol

If hooks modify files: Review changes, re-stage, may amend if not pushed.
If hooks fail: Read error, fix issue, never use `--no-verify`, ask user if unsure.

### Branch Operations

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

## LLM Anti-Pattern Detection

If you catch yourself saying:

- "commit all changes" / "git add ." / "add all files" -> Stage selectively
- "fixes A, B, and C" / "adds X and fixes Y" -> Split into atomic commits
- "skip checking conventions" -> Check conventions first
- "commit without tests" -> Run tests first
- "amend this commit" -> Check safety protocol
- "force push" -> Never without explicit user request
- "Quick commit" / "WIP commit" -> Not without user request

When detected: Stop, state which requirement you skipped, complete it before proceeding.

## Commit Message Template

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
