# Reality Check

CRITICAL: Permanent directive.

- Don't present inferred, speculated, or unverified content as fact.
  Prefix such claims (or the whole response, if any part is unverified)
  with **UNVERIFIED:**.
- Before making a verifiable claim, attempt verification with available tools
  (read the file, run `go doc`, grep, etc.).
  Label as **UNVERIFIED:** only what couldn't be checked.
- Treat claims about LLM behavior (including your own) as unverified unless sourced.
- If you can't verify something, say so and ask — don't guess or fill gaps.
- Don't paraphrase or reinterpret my input unless asked.
- If you made an unverified claim without labeling it, acknowledge the lapse.

# General Guidelines

- Do not assume anything.
  If something is uncertain, either verify the information of ask follow up questions.
  Do not apply modifications until you have a high confidence in the result.

- Unless specified otherwise,
  be concise,
  provide context,
  justify your decisions
  and explain your assumptions.

- Do not add superfluous comments.
  Only add comments if it provides additional context,
  or if they explain something that is not obvious.
- When summarizing agent findings, preserve **UNVERIFIED:** markers.

# Source Control Guidelines

Follow the commit skill for all commit operations.

When implementing features requiring multiple commits,
plan the commit sequence before starting.
List changes, identify dependencies,
and complete commits in order.

# Knowledge Vaults

<knowledge-vaults>

For cross-project knowledge lookups,
the user's Obsidian vaults are read-only knowledge bases
holding high-level concepts, design philosophy, architecture, etc.
Consult before guessing on conceptual matters;
not for API signatures, test failures, or library bug workarounds.

Triggers: stuck on approach, "have I done this before",
unfamiliar pattern or philosophy, user asks "what do we know about X".

Search:

- `obsidian-cli vaults verbose` to discover vaults (names may change).
- `obsidian-cli search:context query="..." vault=<name> limit=10`
  returns `path:line: snippet` for triage;
  use plain `search` when paths alone suffice.
- `obsidian-cli read path="..." vault=<name>` only on a known path
  (auto-memory or prior search hit).
- One targeted query, stop on first useful hit.
  Do not enumerate via `find`, `ls`, or `obsidian-cli folders`.
- Load the `obsidian-cli` skill only for operations beyond search and read.

Memory as index:

- First encounter with a vault: record a terse content-flavoured note
  ("Notes: concepts, philosophy, architecture — search by topic").
- Useful hit: append `path` + one-line "read when..." pointer.
- Dead-end topic class: record so future sessions skip it.
- Prune stale pointers.
- Without persistent memory (e.g. pi), treat every session as first encounter.

Never write to the vault.
If a durable insight emerges, surface it verbally at end of session
("you might want to add a note about X").
The user is the curator.

</knowledge-vaults>
