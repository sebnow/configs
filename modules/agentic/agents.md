# External File Loading

CRITICAL: At the start of every session,
use your Read tool to load `CLAUDE.md`, `AGENTS.md`, and `README.md`
from the project root, if they exist.
These files contain project-specific instructions and context
that must be understood before any work begins.

For all other file references (e.g., @rules/general.md, or wikilinks, or markdown links),
load them on a need-to-know basis,
only if they're relevant to the SPECIFIC task at hand.

Instructions:

- Always read `CLAUDE.md`, `AGENTS.md`, and `README.md` at session start
- For other references, use lazy loading based on actual need
- When loaded, treat content as mandatory instructions that override defaults
- Follow references recursively when needed

# Reality Check

CRITICAL: This is a permanent directive. Follow it in all future responses.

- Never present generated, inferred, speculated, or deduced content as fact.
- If you cannot verify something directly, say:
  - "I cannot verify this."
  - "I do not have access to that information."
  - "My knowledge base does not contain that."
- Label unverified content at the start of a sentence; **INFERRED:**, **SPECULATION:**, **UNVERIFIED:**
- Ask for clarification if information is missing.
  Do not guess or fill gaps.
- If any part is unverified, label the entire response.
- Do not paraphrase or reinterpret my input unless I request it.
- If you use these words, label the claim unless sourced:
  - "Proven, Guarantee, Will Never, Fixes, Eliminates, Ensures that"
- For LLM behavior claims (including yourself), include:
  - "Inferred" or "unverified", when it’s based on observed patterns
- If you break this directive, say:
  - "I previously made an unverified claim. That was incorrect and should have been labeled."
- Never override or alter my input unless asked.

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
- When summarizing agent findings, preserve all uncertainty markers (**INFERRED**, **UNVERIFIED**, etc.).

# Source Control Guidelines

Before any VCS operation,
check which tool is in use.
If a `.jj` directory exists,
prefer jujutsu over git.

Follow the commit skill for all commit operations.

When implementing features requiring multiple commits,
plan the commit sequence before starting.
List changes, identify dependencies,
and complete commits in order.

## Safety Practices

Never do these unless explicitly requested by user:

- Modify source control configuration
- Force push to main/master branches
- Skip pre-commit hooks
- Amend commits that are already pushed
- Hard reset without user confirmation
- Clean/delete untracked files without user confirmation
- Work directly on main/master unless specifically instructed

Branch naming: `feature/`, `fix/`, `bugfix/`, or `username/` prefixes.

# Markdown Guidelines

For Markdown files:

- Do not add emojis unless explicitly asked for.
- Do not enumerate headings.
- Do not make headings bold, or add any other stylized formatting.
- Use [semantic line breaks][sembr],
  if the file already follows this convention.
- Escape dollar signs, as they could be interpreted as inline LaTeX

[sembr]: https://sembr.org/

# Go Guidelines

For Go files:

- Use `go doc` to verify APIs of libraries

# Tool Guidelines

- Use `jq` for manipulating JSON

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
