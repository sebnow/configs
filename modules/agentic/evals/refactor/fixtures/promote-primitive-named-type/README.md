Go moderation service whose public functions identify users and content by
raw `string` IDs.

`BlockUser(userID, contentID string, reporterID string)` and
`AssignReviewer(reviewerID, contentID string)` carry three distinct semantic
kinds — user IDs, content IDs, reporter IDs — all as the same primitive type.
Nothing prevents callers from transposing arguments at the call site: passing
`(contentID, userID, reporterID)` compiles cleanly and silently routes the
moderation action against the wrong subject. The repository wiki notes
historical incidents where exactly this transposition shipped to production.

Validation today happens inside `BlockUser` (`if userID == ""`); there are no
parse constructors and no named types for these IDs, so each new caller has
to re-derive the rules.

`correlationID` in `pkg/log` is a counter-example: it is a per-request opaque
trace value with no domain semantics — it has no parse rules, no equality
beyond string equality, and is never confused with a user or content
identifier. It legitimately stays `string`.
