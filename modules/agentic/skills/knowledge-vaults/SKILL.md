---
name: knowledge-vaults
description: "Consults the user's Obsidian vaults as a read-only knowledge base of concepts, design philosophy, and architecture notes. Use when stuck on an approach, evaluating an unfamiliar pattern, or when the user asks what is known about a topic. Triggers: 'have I done this before', 'what do we know about', 'is there prior art', 'stuck on approach', unfamiliar pattern or philosophy, conceptual research, recall prior thinking. Do NOT use for in-vault edits (use obsidian-cli skill), API signatures, library bug workarounds, or test failures."
---

# Knowledge Vaults

The user's Obsidian vaults hold high-level concepts, design philosophy, and architecture notes — not API references or fix recipes.

## Search

Discover vaults, then search; read only on a known path:

```bash
obsidian-cli vaults verbose
obsidian-cli search:context query="..." limit=10
obsidian-cli read path="..."
```

Specify `vault=<name>` when targeting a vault other than the one implied by the current working directory. One targeted query; stop on first useful hit. Do not enumerate via `find`, `ls`, or `obsidian-cli folders`. See the `obsidian-cli` skill for the full operator reference.

## Memory as Index

- First encounter with a vault: record a terse content-flavoured note ("Notes: concepts, philosophy, architecture — search by topic").
- Useful hit: append `path` + one-line "read when..." pointer.
- Dead-end topic class: record so future sessions skip it.
- Prune stale pointers.
- Without persistent memory (e.g. pi), treat every session as first encounter.

## Curator

Never write to the vault. If a durable insight emerges, surface it verbally at end of session ("you might want to add a note about X").

## Anti-pattern

Pursuing API signatures, fix recipes, or test-failure debug info in the vaults is the wrong tool — use docs, code, or tests instead.
