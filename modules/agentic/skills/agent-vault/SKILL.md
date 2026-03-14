---
name: agent-vault
description: "Persistent cross-project memory using a zettelkasten-style vault of atomic markdown notes connected by wikilinks. Agents store and recall knowledge at $XDG_DATA_HOME/agents/vault/. Use when starting sessions, hitting errors, or when asked to remember/recall/analyze/debrief. Triggers: /vault, 'remember this', 'recall', 'what do we know about', 'check memory', 'vault maintain', 'vault debrief'. Do NOT use for project-specific scratch files or temporary task state."
---

# Agent Vault

Persistent memory for agents across sessions and projects.
Atomic markdown notes connected by wikilinks
form a graph of knowledge that compounds over time.

## Vault Location

Path: `$XDG_DATA_HOME/agents/vault/`
(defaults to `~/.local/share/agents/vault/`).

Directory structure:

```
vault/
  concepts/    # General knowledge: patterns, techniques, solutions
  projects/    # Per-project observations and decisions
    <name>/    # Project name matches repo or directory name
  sessions/    # Session summaries
  archive/     # Superseded notes (bidirectional links to successors)
```

Before first use, create the directory structure:

```bash
mkdir -p "${XDG_DATA_HOME:-$HOME/.local/share}/agents/vault"/{concepts,projects,sessions,archive}
```

## Note Format

Every note is one markdown file. One concept per file.
Filenames are lowercase descriptive slugs: `arena-allocator.md`, `nix-flake-module-pattern.md`.

Required frontmatter:

```yaml
---
created: 2026-03-14
source: claude-code/project-name/session-id
---
```

Optional frontmatter:

```yaml
---
created: 2026-03-14
source: claude-code/project-name/session-id
confidence: high    # high, medium, low
tags: [nix, debugging]
supersedes: "[[previous-note]]"
superseded-by: "[[newer-note]]"
project: project-name
---
```

Body format: structured and terse.
Use bullet points and key-value pairs, not prose.

```markdown
---
created: 2026-03-14
source: claude-code/configs
tags: [nix, flake-parts]
---

# Flake-parts Module Pattern

- Each module is a file in `modules/` directory
- Modules are flake-parts modules, not NixOS modules
- Use `import-tree` to auto-import all modules
- Top-level flake.nix only calls `mkFlake` with imports

Related: [[nix-import-tree]], [[dendritic-pattern]]
```

## Wikilinks

Connect notes using `[[note-filename]]` (without `.md`).
Links create traversable edges in the knowledge graph.

- Link when concepts are genuinely related
- Prefer inline links that read naturally in context
- Bidirectional links are implicit: searching for `[[note-a]]` finds all notes linking to it

## Recall: Reading from the Vault

### When to Recall

Recall automatically at:

1. **Session start** —
   Search for notes matching the current project name.
   Read `projects/<name>/` and follow wikilinks to relevant concepts.
2. **Errors and blockers** —
   When hitting an error, test failure, or unfamiliar pattern,
   search the vault for matching terms before debugging from scratch.

Recall on demand: `/vault recall <query>`

### How to Recall

```bash
VAULT="${XDG_DATA_HOME:-$HOME/.local/share}/agents/vault"
```

1. **Project notes**: list and read files in `$VAULT/projects/<name>/`
2. **Keyword search**: `grep -rl "<query>" "$VAULT"/{concepts,projects} --include='*.md'`
3. **Wikilink traversal**: when a note references `[[other-note]]`,
   read `$VAULT/concepts/other-note.md` or search for `other-note.md` in the vault
4. **Skip archived notes**: exclude `archive/` from search results by default.
   Only consult archive when tracing decision history.

Before acting on recalled information,
verify it against current project state.
Vault notes may be outdated.

## Remember: Writing to the Vault

### When to Remember

Write to the vault when you discover:

- A non-obvious project convention or constraint
- A solution to a problem that required significant investigation
- A pattern or technique that applies beyond the current task
- An error whose root cause was not immediately apparent

On demand: `/vault remember <topic>`

### What to Write

Prefer general concepts over project-specific details.
If a discovery applies beyond one project,
write it to `concepts/`.
If it's project-bound, write it to `projects/<name>/`.

Separate the general from the specific:
a debugging technique goes in `concepts/`,
while "this project's CI requires Node 20" goes in `projects/`.

### How to Write

1. Choose the right directory (`concepts/` or `projects/<name>/`)
2. Use a descriptive slug filename
3. Include required frontmatter (`created`, `source`)
4. Write terse, structured content (bullets, not prose)
5. Add wikilinks to related existing notes
6. Prefer creating new notes over editing existing ones

Before creating a note, search the vault
for existing notes on the same topic.
If one exists, extend it rather than creating a duplicate.

## Analyze: Project Scanning

On `/vault analyze` or when exploring an unfamiliar project:

1. Scan the project structure, conventions, and key patterns
2. Write or update project notes in `projects/<name>/`
3. Extract general patterns into `concepts/` notes
4. Link project notes to relevant concepts

This builds the project's knowledge base for future sessions.

## Debrief: Session Summaries

At session end or on `/vault debrief`:

1. Review what was accomplished, what was learned, what failed
2. Write a session note to `sessions/<project>-<date>.md`
3. Extract any general insights into separate concept notes
4. Link the session note to relevant project and concept notes

Session notes capture ephemeral context.
Concept notes capture durable knowledge.
Always extract concepts from sessions — the session note alone
is a dead end that won't be found by keyword search.

## Truth Maintenance

Traversal is not truth maintenance.
A note being linked does not mean it is current.

### When You Find Outdated Information

If you read a vault note and discover its content
no longer matches reality:

1. Create a successor note with the corrected information
2. Add `supersedes: "[[old-note]]"` to the successor's frontmatter
3. Add `superseded-by: "[[new-note]]"` to the old note's frontmatter
4. Move the old note to `archive/`

This preserves decision history —
the chain of superseded notes shows how understanding evolved.

### Verification During Recall

When recalling notes at session start or during work:

- Check `created` date — older notes are more likely stale
- Check `confidence` — low-confidence notes need verification
- If a note has `superseded-by`, follow the chain to the current version
- Verify claims against current project state before acting on them

Never silently act on vault information without verification.
Stale notes mislead more than missing notes.

## Maintain: Vault Hygiene

On `/vault maintain` or when you notice issues during reads:

- **Duplicates**: find notes covering the same concept, merge into one
- **Orphans**: notes with no inbound or outbound wikilinks.
  Either link them or consider if they're still relevant.
- **Broken links**: wikilinks pointing to non-existent notes.
  Create the missing note or remove the link.
- **Stale archive**: `archive/` notes older than 6 months
  with no inbound links can be deleted

Do not run maintenance automatically.
Suggest it when you notice problems during normal vault use.

## Common Issues

### Empty vault on first session

The vault starts empty. On first use with a project,
run `/vault analyze` to populate it.
Subsequent sessions benefit from accumulated knowledge.

### Too many search results

If keyword search returns too many matches,
narrow with tags or project scope:

```bash
grep -rl "<query>" "$VAULT/projects/<name>" --include='*.md'
```

### Note already exists

Before writing, always search first.
If a note exists on the topic, append to it
rather than creating a duplicate.

## Examples

### Example: Session start recall

Trigger: agent starts a session in the `configs` project.
Actions:
1. List files in `$VAULT/projects/configs/`
2. Read each note, follow wikilinks to concepts
3. Build context from recalled notes before starting work

### Example: Error-triggered recall

Trigger: agent hits "module not found" error in a Nix build.
Actions:
1. Search vault: `grep -rl "module not found" "$VAULT"/{concepts,projects} --include='*.md'`
2. Find `concepts/nix-module-import-errors.md`
3. Read note, verify advice applies, use solution

### Example: Remember a discovery

Trigger: agent discovers the project uses a non-standard test runner.
Actions:
1. Search vault for existing note on the topic
2. No match found — create `$VAULT/projects/myapp/test-runner-config.md`
3. Write terse note with frontmatter, link to `[[jest-alternatives]]` if relevant

### Example: Archiving a stale note

Trigger: agent reads `concepts/node-18-compatibility.md`, discovers project upgraded to Node 22.
Actions:
1. Create `concepts/node-22-compatibility.md` with updated info
2. Add `supersedes: "[[node-18-compatibility]]"` to new note
3. Add `superseded-by: "[[node-22-compatibility]]"` to old note
4. Move old note to `archive/node-18-compatibility.md`
