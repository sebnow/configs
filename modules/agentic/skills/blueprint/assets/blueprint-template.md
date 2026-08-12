# Blueprint Template

Sections in this order:

## Context

What is changing and why.
Condense the decided context into one or two paragraphs.
Plain language. State the change and its motivation; leave the design to the next section.

## Design

Module interfaces and the call flow for the change.
Every call named in the tree corresponds to a module signature,
so the two subsections cross-check each other.

### Modules

For each module the change introduces or modifies:

#### `Server.ListFeed` (Go)

```go
func (s *Server) ListFeed(ctx context.Context, userID string) ([]FeedItem, error)
```

Responsibility: returns a user's feed with each item's comments attached.

Contracts: returns an empty slice (not nil) when the feed is empty; read-only.

#### `Store.FindCommentsByItems` (Go)

```go
func (s *Store) FindCommentsByItems(ctx context.Context, itemIDs []string) (map[string][]Comment, error)
```

Responsibility: batch-loads comments for many items in a single query.

Contracts: keys the result by item ID; items with no comments are absent from the map.

Include only signature declarations, type aliases, function or method headers,
and interface or struct definitions.
Leave function bodies, full schemas with column lists, configuration values,
file paths, and line numbers to the downstream issues.

Organize this subsection so issues can be sliced vertically —
each module entry should map to one or more end-to-end issues.

### Call Flow

Map how control flows through the change as an indented tree.
The root is the entry point; children are the calls it makes, in order.
Prefix every line with a marker column, unified-diff style: a space for unchanged
calls, `+` for added calls, `-` for removed calls. The content (with its tree
indentation) follows the marker, so a ```diff block highlights it.

The example below replaces a per-item query (an N+1) with a single batch load;
its calls map to the module signatures above.

```diff
 GET /feed handled
   Server.ListFeed(ctx, userID)
     Store.FindFeedItems(ctx, userID)
-    for each item:
-      Store.FindComments(ctx, item.ID)
+    Store.FindCommentsByItems(ctx, itemIDs)
     writeJSON(w, items)
```

If the change alters no control flow (pure data or schema change),
write "No control-flow change" and let the Modules subsection stand alone.

## Decisions

The architecture and design decisions made for this change, each with its rationale.
A change may involve several decisions; record each one and why it was chosen over the alternatives.
A permanent, cross-cutting decision belongs in an ADR — reference it here rather than restating it.

## Testing

- Which modules from Design get automated tests
- Prior-art reference: tests in the codebase that demonstrate the style for this kind of module
- Behavior-vs-implementation guidance:
  what to assert (observable behavior) vs. what not to assert (internal structure)

## Out of Scope

Explicitly list what this change will NOT include.

This section must contain at least one non-trivial exclusion —
something a reasonable reader might otherwise assume is in scope.
Empty or filler exclusions defeat the purpose.

Example:

```
Out of scope for this change:
- Migrating existing sessions to the new service layout
- Concurrent session limits per user
- Metrics for session open/close latency
```

## Open Questions

Anything still unclear.
This section should be empty — calibration happens before the document.
If it has entries, stop and calibrate (run the brainstorm skill) before implementation.
