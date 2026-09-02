# PR Description Template

The register governs the prose. This template governs the structure.
Headings are optional. Use them only when length warrants navigation.
Favor short paragraphs and lists over dense blocks. For a short PR, the summary
paragraph alone is the whole description.

## Summary (always)

Lead with what the change does and why, in a sentence or two, using the
implicit subject. Keep paragraphs short; when the change has several distinct
parts, list them rather than packing them into one paragraph.

> Bounds the math path's upload concurrency to match the non-math budget.
> The math path previously spawned one upload per tile with no ceiling, so a
> large document could open hundreds of connections at once and exhaust the pool.

## Call Flow (non-trivial only)

A diff-marked tree derived from the diff. Space for unchanged, `+` added, `-` removed.

```diff
 handleUpload(doc)
   splitTiles(doc)
-  for each tile:
-    upload(tile)
+  uploadBounded(tiles, budget)
```

Omit this section when the change alters no control flow.

## Modules (non-trivial only)

One bullet per touched module: name plus a one-line responsibility.

- `uploadBounded` — uploads tiles under a shared concurrency budget.
- `splitTiles` — divides a document into upload units.

## Notes (optional)

Anything a reviewer needs that the diff does not show: a deliberate trade-off,
a follow-up left out of scope, a manual verification step. Skip when there is
nothing to add.
