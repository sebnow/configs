---
name: create-pr
description: "Turns the current branch's committed work into a pull request: writes the description in a conversational reference-document register and, for non-trivial changes, carries forward a diff-marked call-flow tree and a touched-module list. Creates a draft by default; publishes only on explicit request. Triggers: 'create a PR', 'open a pull request', 'raise a PR', 'gh pr create', 'draft a PR'. Do NOT use to write commits (use the commit skill) or to plan an undecided change (use brainstorm/blueprint)."
disable-model-invocation: true
---

# Create PR

Turn the current branch's committed work into a pull request.

Assumes the commits already exist. Committing belongs to the commit skill.
The code is the source of truth. If a blueprint exists for this change,
use it for context, but describe what the diff actually does.

## Workflow

1. **Detect the base branch.** Use the branch's tracking ref when it is not the
   remote default; otherwise use the remote default. State the chosen base and
   let the user override.
2. **Read the diff.** Compare the branch against the base
   (`git diff <base>...HEAD`, `git log <base>..HEAD`). This is the source of truth.
3. **Judge triviality.** A trivial PR touches one module and alters no control flow.
   Non-trivial PRs get the call-flow and modules sections; trivial PRs do not.
4. **Write the description** using the register below and
   [assets/pr-description-template.md](assets/pr-description-template.md).
5. **Create the PR.** Draft by default. Publish only on explicit request,
   and confirm before publishing (see Publish gate).

## Register

Write **conversational**.

Unlike Slack and code review, PR descriptions are reference documents read
asynchronously, often by reviewers who were not in the conversation.
Lists, code blocks, and hyperlinks are appropriate where the content calls for them.

Default to flowing paragraphs. Reach for section headings only when the
description is long enough that navigation aids the reviewer — short PR
descriptions read better as continuous prose.

Get the point across quickly, using simplified technical English (ASD-STE100).
Elaborate after the main point is made, if the topic may be unclear.

Use implicit subject; avoid "I" and "we". The work and the changes are the subject.

Good:
> Bounds the math path's upload concurrency to match the non-math budget.

Avoid:
> I bounded the math path's upload concurrency.

The title follows the same register: imperative, implicit subject, matching the
commit skill's message style.

## Call flow and modules (non-trivial PRs only)

Mirror the blueprint skill's two structural aids. Derive both from the diff.

### Call flow

Map how control flows through the change as an indented tree. The root is the
entry point; children are the calls it makes, in order. Prefix every line with a
marker column, unified-diff style: a space for unchanged calls, `+` for added,
`-` for removed. A ```diff block then highlights the change.

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
- `Server.ListFeed` — returns a user's feed with comments attached.
- `Store.FindCommentsByItems` — batch-loads comments for many items in one query.
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

- Invent a ticket or issue reference — this skill does not link tickets.
- Describe intent from a blueprint that the diff does not show; the code wins.
- Add the call-flow or modules sections to a trivial PR.
- Publish a non-draft PR without explicit user go-ahead.
