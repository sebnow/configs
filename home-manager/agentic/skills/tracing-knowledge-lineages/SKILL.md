---
name: tracing-knowledge-lineages
description: "Use when replacing approaches or dismissing patterns. Traces decision history through git/jj logs, commit messages, and decision records to avoid repeating failures and rediscover abandoned solutions. Triggers: refactoring, replacing patterns, 'simplify', 'modernize', 'outdated', 'legacy', 'clean up', architectural changes."
---

# Tracing Knowledge Lineages

## Overview

Ideas have history.
Understanding why we arrived at current approaches prevents repeating failures and rediscovers abandoned solutions.

Core principle: Before changing existing approaches, understand their lineage.

## When to Use

Required before:

- Replacing existing configurations or approaches
- Dismissing patterns as "old" or "outdated"
- Implementing "new" ideas or architectural changes
- Declaring current best practices

Activation trigger phrases:

- "Let's simplify this"
- "This seems outdated"
- "We should modernize"
- "Why is this so complex?"
- "Let's rewrite this"
- "This is legacy code"
- "Let's clean this up"
- "Let's refactor this"

When you think any of these, trace the lineage first.

## Investigation Techniques

### Decision Archaeology

Search for context explaining current state:

1. Check commit messages for the relevant files:
   ```bash
   # Git
   git log --all --full-history -- path/to/file

   # Jujutsu
   jj log path/to/file
   ```

2. Search history for keywords:
   ```bash
   # Git
   git log --all --grep="pattern_name"
   git log --all -S"specific_code_or_config"

   # Jujutsu
   jj log -r 'description(pattern_name)'
   jj log -r 'diff_contains("specific_code_or_config")'
   ```

3. Look for decision records or documentation:
   - Architecture Decision Records (ADRs)
   - CHANGELOG entries
   - Comment blocks explaining rationale

4. Check related issues or discussions:
   - Issue trackers
   - Pull request discussions
   - Code review comments

### Failed Attempt Analysis

When considering approach X:

1. Search for previous attempts:
   ```bash
   # Git
   git log --all --grep="X"
   git log --all -S"X" --diff-filter=D

   # Jujutsu
   jj log -r 'description(X)'
   jj log -r 'diff_contains("X")'
   ```

2. If found, understand:
   - Why was it tried?
   - Why did it fail?
   - Has the context changed since then?

3. State findings explicitly to user before proceeding

### Revival Detection

Before implementing "new" approaches:

1. Check if it's actually old:
   - Search history for similar patterns (git/jj log)
   - Look for deleted implementations
   - Check why they were removed

2. Evaluate changed context:
   - Do old failure reasons still apply?
   - What's different now?
   - What was learned?

3. Document why revival is appropriate

### Paradigm Shift Mapping

When major architectural transitions occurred:

1. Identify transition points in history (git/jj log)
2. Understand what triggered the shift
3. Document what was gained and lost
4. Evaluate if reverse shift is warranted

## Required Protocol

Before making changes:

1. Search history for relevant context (git/jj log)
2. Read commit messages completely
3. Identify decision points and rationale
4. State findings explicitly to user:
   - What was tried before?
   - Why current approach was chosen?
   - What constraints influenced decisions?

5. Evaluate if change is still appropriate given history

Only after completing investigation and stating findings may you propose changes.

## Red Flags

Stop immediately if you catch yourself thinking:

- "Let's just rewrite this" → Trace lineage first
- "This is obviously outdated" → Age ≠ quality
- "Nobody does it this way anymore" → Why did they do it?
- "This is too complex" → Understand why before simplifying
- "We should use the new pattern" → Newness ≠ quality
- "This is legacy code" → Understand why it exists
- "Context has changed, so..." → Provide evidence

All of these mean: Stop. Investigate history first.

## Permission to Override History

You may proceed without full lineage investigation only when:

1. Context fundamentally changed (with evidence):
   - Technology limitations no longer apply (cite version/feature)
   - Requirements shifted substantially (cite decision record)
   - Dependencies evolved significantly (cite changelog)

2. Critical lessons learned (with evidence):
   - Security vulnerability discovered (cite CVE/advisory)
   - Performance issue identified (cite benchmarks)
   - Better approach proven in production (cite metrics)

3. Original reasoning was flawed (with evidence):
   - Based on incorrect assumptions (cite what was wrong)
   - Violated known best practices (cite standards)
   - Created technical debt (cite impact)

You must state explicitly why you're overriding history with supporting evidence.
Do not claim override conditions apply without evidence.

## Integration with Other Skills

Use this skill before:

- source-control-hygiene: Understanding why conventions exist
- systematic-debugging: Checking if issue was previously fixed
- Any refactoring or architectural changes

## Examples

### Good: Investigating Before Changing

```
User: This Nix configuration seems overly complex, let's simplify it.
Agent: Let me check the history first.

[Searches log for the file: jj log path/to/config.nix]
[Finds commit: "Work around NixOS module conflict with home-manager"]
[Reads commit message explaining the issue]

Agent: This complexity was added to work around a module conflict.
The commit message from 6 months ago explains that the simpler
approach caused build failures. Has that issue been resolved?
```

### Bad: Changing Without Context

```
User: This Nix configuration seems overly complex, let's simplify it.
Agent: I'll simplify this configuration.

[Removes "complex" workaround]
[Breaks build]
[Discovers workaround was necessary]
[Reverts change]
```

## LLM Anti-Pattern Detection

If you output these phrases before investigating lineage:

- "I'll simplify..." → Stop. Check history first.
- "This is outdated..." → Stop. Verify with log.
- "Let's modernize..." → Stop. Understand current approach.
- "We should use X instead..." → Stop. Was X tried before?

When detected: Stop, investigate lineage, then proceed with context.
