# Label Poisoning in Anti-Pattern Sections

When a token — a word, command, phrase, concept, or structural
element — appears under a "Bad" label in a prompt,
agents avoid that token wholesale.
A nearby "Good" variant containing the same token is ignored.
This is label poisoning.

The effect is symbol-level, not semantic.
The agent learns "avoid this surface form" rather than
"avoid this usage of this surface form".

## The Rule

When writing anti-patterns,
name the specific dangerous condition or invocation,
not the underlying token.

If the token has both a dangerous and a safe form,
show the safe form first in a positive section.
Only the truly dangerous variant belongs in the anti-pattern.

Never put a token under a "Bad" label
when a valid use of that token exists.

## Examples Across Contexts

### Tool invocation

Wrong — agents avoid `deploy` entirely:

```markdown
# Bad: opens confirmation dialog
deploy staging
# Good: non-interactive
deploy --yes staging
```

Right — safe usage shown first; anti-pattern names the condition:

```markdown
## Deployment
Always use non-interactive mode:
deploy --yes staging

## Anti-Patterns
- Run deploy without --yes in scripts
  (blocks automation)
```

### Prose and reply formatting

Wrong — agents avoid the word "obviously" in all contexts,
even legitimate uses:

```markdown
Bad word: "obviously"
Good: "as shown by..."
```

Right — name the failure mode, not the word:

```markdown
Avoid hedges that dismiss the reader's question
("obviously", "just", "simply") when the answer is non-trivial.
Use them when describing established facts.
```

### Concepts and techniques

Wrong — agents avoid chain-of-thought entirely:

```markdown
Bad: chain of thought
Good: direct answer
```

Right — name the condition where the technique fails:

```markdown
Skip chain of thought for tasks the model handles
in a single step; the extra reasoning adds noise.
Use it for multi-step derivations.
```

### API or library names

Wrong — agents avoid the API even for valid uses:

```markdown
Bad: use of `eval`
```

Right — bound the prohibition to the unsafe condition:

```markdown
Never call `eval` on untrusted input.
Reserve `eval` for trusted compile-time templates.
```

## Decision Rule

Before writing an anti-pattern, ask:
does the token under the "Bad" label have a legitimate use?

If yes, rewrite the anti-pattern to name the condition
that makes the use illegitimate. The token itself stays neutral.

If no — the token is unconditionally dangerous —
the "Bad" label is safe to apply.
