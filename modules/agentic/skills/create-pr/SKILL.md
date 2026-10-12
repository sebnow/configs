---
name: create-pr
description: "Turns the current branch's committed work into a pull request: writes the description in a conversational reference-document register and, for non-trivial changes, carries forward a diff-marked call-flow tree and a touched-module list. Creates a draft by default; publishes only on explicit request. Triggers: 'create a PR', 'open a pull request', 'raise a PR', 'gh pr create', 'draft a PR'. Do NOT use to write commits."
---

# Create PR

Turn the current branch's committed work into a pull request.

Assumes the commits already exist. The code is the source of truth. The user
should have already provided context around the change to provide the reasoning
for it, but describe what the diff actually does.

## Workflow

1. **Detect the base branch.** Use the branch's tracking ref when it is not the
   remote default; otherwise use the remote default. State the chosen base and
   let the user override.
2. **Read the diff.** Compare the branch against the base. This is the source of truth.
3. **Judge triviality.** A trivial PR touches one module and alters no control flow.
   Non-trivial PRs get the call-flow and modules sections; trivial PRs do not.
4. **Write the description** using the register below and
   [assets/pr-description-template.md](assets/pr-description-template.md).
5. **Create the PR.** Draft by default. Publish only on explicit request,
   and confirm before publishing (see Publish gate).

## Register

PR descriptions are reference documents read asynchronously, often by reviewers
who were not in the conversation. Lists, code blocks, and hyperlinks are
appropriate where the content calls for them. Never hard-wrap the description.

Favor legibility over density. A wall of dense prose buries the point. Lead with
the main point in a sentence or two, then break the rest into short paragraphs,
lists, and — once length warrants navigation — section headings, so a reviewer
can scan the description and get the main points quickly.

Reach for a bulleted list whenever the change has discrete parts: several fixes,
a set of touched behaviors, a handful of trade-offs. A list carries these better
than a run-on sentence. Keep paragraphs short and separate them with a blank
line; a short PR may need only a lead paragraph.

Get the point across quickly. Elaborate after the main point is made, if the
topic may be unclear.

Use implicit subject; avoid "I" and "we". The work and the changes are the
subject.

Good:

> Bounds the math path's upload concurrency to match the non-math budget.

Avoid:

> I bounded the math path's upload concurrency.

Frame the description as a change to the current code, not as documentation of
the resulting code. Describe what the PR alters (the delta) rather than
narrating how the post-merge code behaves as if it were the standing baseline.

Good:

> Adds a short-lived cache in front of the profile lookup.

Avoid:

> The profile lookup reads through a short-lived cache.

Avoid these habits:

- Significance framing — state the change, not its novelty or importance. An
  additive verb already implies absence, so "Adds X it never had" restates the verb.
  Drop "it never had", "for the first time", "finally", "long-missing".
- Setup-then-reveal colon — do not withhold the change behind a framing clause and a
  colon. Write "Adds `/metrics` and `/health` endpoints", not "Gives the service an
  HTTP surface it never had: ...". A colon introducing a genuine list is fine.
- Noun stacks — unpack into a verb or relative clause; chain at most two modifiers
  before a noun. Write "a gauge reporting River job counts", not "a River job-state
  gauge".

The title follows the same register: imperative, implicit subject.

## Call flow and modules (non-trivial PRs only)

Non-trivial PRs carry structural aids, derived from the diff alone

- a diff-marked call-flow tree
- a touched-module list.

### Call flow

Map how control flows through the change as an indented tree. The root is the
entry point; children are the calls it makes, in order.

Prefix every line with a marker column, unified-diff style: a space for
unchanged calls, `+` for added, `-` for removed. A diff code fence highlights
the change.

```diff
 GET /feed handled
   Server.ListFeed(ctx, userID)
     Store.FindFeedItems(ctx, userID)
-    for each item:
-      Store.FindComments(ctx, item.ID)
+    Store.FindCommentsByItems(ctx, itemIDs)
     writeJSON(w, items)
```

If the change alters no control flow, omit this section.

### Modules

One bullet per touched module: name plus a one-line responsibility. The diff
itself shows the contracts, so do not restate signatures or error modes.

```
- `Server.ListFeed`: returns a user's feed with comments attached.
- `Store.FindCommentsByItems`: batch-loads comments for many items in one query.
```

## Creating the PR

Assume `gh` is installed and authenticated. Create a draft:

```
gh pr create --draft --base <base> --title <title> --body-file <file>
```

Write the body to a temp file rather than inlining it, so paragraphs and diff
blocks survive shell quoting.

If `gh` is unavailable, print the title and body as a fenced block for manual paste.

## Publish gate

Draft is the default; create it without prompting.

Publishing is outward-facing. When the user asks to publish (a non-draft PR, or
marking an existing draft ready), confirm before running `gh pr create` without
`--draft` or `gh pr ready`.

## Anti-patterns

Never:

- Invent a ticket or issue reference.
- Hard-wrap the description body. Paragraphs are single unwrapped lines.
- Describe intent from source documents that the diff does not show; the code wins.
- Add the call-flow or modules sections to a trivial PR.
- Publish a non-draft PR without explicit user go-ahead.
