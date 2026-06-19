---
name: obsidian-cli
description: "Provides obsidian-cli for Obsidian vault operations. Use when creating, moving, renaming, or deleting vault notes; searching vault content; querying or setting frontmatter properties; or working with tags. Covers composition guidance and operator reference for obsidian-cli subcommands. Triggers: 'create note', 'move note', 'rename note', 'delete note', 'search vault', 'find notes', 'obsidian', 'vault', 'tag search', 'property query', 'frontmatter'. Do NOT use for non-vault markdown files."
compatibility: Requires obsidian-cli on PATH and a running Obsidian instance.
---

# Obsidian CLI

Use `obsidian-cli` for all Obsidian vault operations.
`obsidian-cli` uses Obsidian's index, handles backlink updates,
and applies Templater folder rules automatically.

Get command usage via `obsidian-cli <command>` with no arguments.

## File Operations

### Create

```bash
obsidian-cli create path="Notes/Note Title.md" content="..."
```

Pass `path=` alone to `create`.
Pass the full body in the same `create` call via `content=`.
Use `\n` for newlines and `\t` for tabs inside the content value.
Creating an empty file then filling it is two operations that can desync.

Templater folder rules apply templates automatically by path.
Use `template=` only when overriding the default.

After creating a note, use Edit or the md tool to modify its content.

### Append and Prepend

```bash
obsidian-cli append path="Notes/Note Title.md" content="..."
obsidian-cli prepend path="Notes/Note Title.md" content="..."
```

Use `append` only to add to a file that already has content.

### Move

```bash
obsidian-cli move file="Note Title" to="Projects/"
```

`file=` resolves by name (wikilink-style, no `.md` extension needed).
`path=` is an exact vault-relative path (`folder/note.md`).

### Rename

```bash
obsidian-cli rename file="Note Title" name="New Title.md"
```

### Read

```bash
obsidian-cli read path="Notes/Note Title.md"
```

### Delete

```bash
obsidian-cli delete path="Notes/Note Title.md"
```

## Searching

```bash
obsidian-cli search query="phrase" limit=20
obsidian-cli search:context query="phrase"
obsidian-cli search query="phrase" path="Projects"
obsidian-cli search query="phrase" format=json
```

`format=json` returns a flat JSON array of path strings — not objects.
Extract paths with `jq -r '.[]'`, not `.[].path`:

```bash
obsidian-cli search query="rabbitmq" format=json | jq -r '.[]'
```

The `query=` value accepts Obsidian's full search syntax.

### Text and Boolean Operators

| Query | Meaning |
| --- | --- |
| `query="project notes"` | AND (both must appear) |
| `query="project OR notes"` | OR |
| `query="\"team meeting\""` | exact phrase |
| `query="project -draft"` | exclude term |
| `query="meeting (work OR personal)"` | grouped OR |

Multi-word values without inner quotes are AND content matches across body prose, not exact-phrase or filename matches. Wrap with escaped quotes (`query="\"exact phrase\""`) for the literal phrase, or use the `file:` operator below to match against filenames.

### Search Operators

| Operator | Purpose | Example |
| --- | --- | --- |
| `file:` | Match filename | `file:.jpg` |
| `path:` | Match file path | `path:"Daily notes/2022"` |
| `content:` | Match content explicitly | `content:"happy cat"` |
| `tag:` | Match tag (not in code blocks) | `tag:#work` |
| `line:` | Both terms on same line | `line:(mix flour)` |
| `block:` | Both terms in same block | `block:(dog cat)` |
| `section:` | Both terms under same heading | `section:(dog cat)` |
| `task:` | Match in any task | `task:call` |
| `task-todo:` | Match uncompleted task | `task-todo:call` |
| `task-done:` | Match completed task | `task-done:call` |
| `match-case:` | Case-sensitive | `match-case:HappyCat` |
| `/regex/` | JavaScript regex | `/\d{4}-\d{2}-\d{2}/` |

`tag:` does not traverse tag hierarchies — `tag:#parent` does not match `#parent/child`.
`block:` is slower than other operators (parses all Markdown).

### Checking Whether a Note Exists

1. **By known path** — `obsidian-cli file path="Notes/Example Topic.md"`. Returns metadata for the note, or prints `Error: File "<p>" not found.` to stdout when absent. Exit code is 0 either way — detect absence by checking the output, not `$?`.

2. **By title only** — `obsidian-cli search query='file:"Example Topic"' format=json | jq -r '.[]'`. The `file:` operator matches against filenames, so the result set is not diluted by body-prose matches. Empty array means no such note.

A bare multi-word `search query=` is a content search — it will both miss exact-title notes whose body does not repeat the title, and bury the right note among AND-matches across body prose. Reach for `file` or the `file:` operator instead when you only want to know whether a note exists.

### Property (Frontmatter) Queries

Use bracket syntax inside `query=`:

| Query | Meaning |
| --- | --- |
| `[status]` | Files that have the `status` property |
| `[status:"In Progress"]` | Files where status equals "In Progress" |
| `[status:null]` | Files where status exists but has no value |
| `[status:TODO OR "In Progress"]` | OR within property value |
| `[author:null]` | Property is unset |

`null` matches truly blank properties (`status: `) but not `""` or `[]`.

Combine with text: `query="[type:article] climate"`

### Tag Commands

```bash
obsidian-cli tag name="#project" verbose
obsidian-cli tag name="#project" total
obsidian-cli search query="tag:#project meeting"
```

## Properties

```bash
obsidian-cli property:read path="Notes/Note.md" name="status"
obsidian-cli property:set path="Notes/Note.md" name="status" value="Done"
obsidian-cli property:remove path="Notes/Note.md" name="status"
obsidian-cli properties path="Notes/Note.md"
```

## Other Useful Commands

```bash
obsidian-cli backlinks path="Notes/Note.md"
obsidian-cli links path="Notes/Note.md"
obsidian-cli orphans
obsidian-cli deadends
obsidian-cli unresolved
obsidian-cli recents
obsidian-cli wordcount path="Notes/Note.md"
obsidian-cli outline path="Notes/Note.md"
obsidian-cli tasks
obsidian-cli files path="Notes"
obsidian-cli folders
```

