# PRD Template

Required sections in this order:

## Problem Statement

State the problem and who has it.
Plain language. No solutions yet.

## Solution

Describe the chosen approach in one or two paragraphs.
Reference decisions captured in the brainstorm.
Avoid implementation detail — interfaces and contracts go in Implementation Decisions.

## User Stories

Describe how users will interact with the feature.
Format: "As a [user type], I want to [action] so that [benefit]"

Include realistic usage scenarios.

## Out of Scope

Explicitly list what this feature will NOT include.

This section must contain at least one non-trivial exclusion —
something a reasonable reader might otherwise assume is in scope.
Empty or filler exclusions defeat the purpose.

This section sits above implementation details deliberately:
scope decisions constrain everything that follows.

Example:

```
Out of scope for this release:
- AI-powered priority suggestions (postpone to next quarter)
- Bulk priority assignment via CSV import
- Priority history and audit trail
- Mobile-specific priority gestures
```

## Implementation Decisions

Captures the module sketch and any architectural decisions made during brainstorm.

Required content:

- Modules to build or modify, with their public interface signatures and prose contracts
  — from the module sketch
- Architectural decisions and the reasoning behind them
- Cross-cutting contracts (auth, persistence, error handling) if relevant

Each module entry follows this shape:

> **RankFeed** (Go service)
>
> ```go
> func RankFeed(ctx context.Context, userID string, cursor string) ([]FeedItem, error)
> ```
>
> Contracts: returns an empty slice (not nil) when the feed is empty;
> returns `ErrCursorExpired` if the cursor is older than 24 h;
> no side effects.

> **PriorityBadge** (TypeScript UI)
>
> ```ts
> type PriorityBadge = (props: { priority: 1 | 2 | 3 | 4; label: string }) => JSX.Element
> ```
>
> Contracts: renders a colored chip; throws if `priority` is outside 1–4.

Permitted: signature declarations, type aliases, function/method headers, interface or trait definitions.
Forbidden: function bodies, full schemas with column lists, configuration values, file paths, line numbers.

Organize this section so issues can be sliced vertically —
each module entry should map to one or more end-to-end issues
that exercise the module from a user-visible behavior.

## Testing Decisions

Captures which modules get automated tests and what behavior to verify.

Required content:

- Which modules from Implementation Decisions get tests
- Prior-art reference: tests in the codebase that demonstrate
  the testing style for this kind of module
- Behavior-vs-implementation guidance:
  what to assert (observable behavior)
  vs. what not to assert (internal structure)

## Open Questions

List anything still unclear after brainstorm.
These must be resolved before implementation begins.

## Success Criteria

How will we know this feature works?
Include measurable indicators.

Example:

```
- 60% of users assign priorities within first week
- Task selection time decreases by 30%
- Zero priority-related bugs in first month
```
