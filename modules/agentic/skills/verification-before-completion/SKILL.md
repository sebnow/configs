---
name: verification-before-completion
description: "MUST load after making code changes and before claiming done. Run the project build/test command and confirm exit 0 before any completion claim, commit, or push. If verification fails, use the diagnostic output to fix the problem and re-verify. Triggers: editing source files, writing code, modifying config, making changes, implementing features, fixing bugs."
---

# Verification Before Completion

Core principle: Evidence before claims, always.

## When To Apply

MUST apply before:
- Any commit or push operation
- Any completion claim ("done", "finished", "complete", "fixed")
- Any summary of completed work to the user
- Moving to the next task after finishing one

## Verification Protocol

Before claiming any status:

1. Detect: What build/test command does this project use?
2. Run: Execute the command (fresh, complete)
3. Read: Full output, check exit code, count failures
4. Confirm: Does output confirm success?
   - If yes: State result with evidence. Proceed.
   - If no: Read the diagnostic output. Fix the problem. Re-run from step 2.
   - If tool unavailable: Stop and tell the user.
5. Only then: Make the claim, commit, or declare done

Skip any step = no claim permitted.

### When Verification Cannot Run

If the verification command is not available in the environment
(e.g., `nix: command not found`),
this is not an exemption.
Do not label the work "UNVERIFIED" and proceed anyway.
Stop and tell the user the verification command is unavailable.

### Detecting the Verification Command

Check the project root for these files and run the corresponding command:

| File | Command |
|------|---------|
| `flake.nix` | `nix flake check` or `nix build` |
| `go.mod` | `go build ./...` and `go test ./...` |
| `Cargo.toml` | `cargo build` and `cargo test` |
| `build.zig` | `zig build test` |
| `package.json` | `npm test` |
| `Makefile` | `make check` or `make test` |
| `pyproject.toml` | `pytest` |

If multiple apply, run all that are relevant to the changed files.
If none apply, state explicitly: "No build/test command found for this project."
Do not treat absence of a build tool as permission to skip verification.

## Red Flags

Stop immediately if you are about to:

- Say "should", "probably", "seems to" instead of running verification
- Express satisfaction before verification ("Great!", "Perfect!", "Done!")
- Commit or push without running the build/test command
- Claim "my tests pass" when the full suite has failures
- Say "looks correct" based on reading code instead of executing it
- Rationalize skipping verification ("simple change", "config only", "just docs")

## Common Failures

| Claim | Requires | Not Sufficient |
|-------|----------|----------------|
| Tests pass | Full suite: 0 failures | "My new tests pass" |
| Build succeeds | Build command: exit 0 | Reading the diff |
| Bug fixed | Test reproducing symptom: passes | Code changed |
| Config works | Config evaluates without error | "Syntax looks valid" |

## Verification Patterns

Build:
```
Good: Run `nix flake check` -> exit 0 -> "Build passes"
Bad:  Read the diff -> "Looks correct" -> commit
```

Tests:
```
Good: Run full test suite -> 34/34 pass -> "All tests pass"
Bad:  "My new tests pass" (ignoring pre-existing failures)
```

## Bottom Line

Run the command. Read the output. Then claim the result.

No exceptions.
