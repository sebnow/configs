---
name: obsidian-cli
description: "Provides obsidian-cli for Obsidian vault operations. Use when creating, moving, renaming, or deleting vault notes; searching vault content; querying or setting frontmatter properties; or working with tags. Always prefer obsidian-cli over Write, Edit, Bash mv/mkdir, grep, find, or rg for vault files. Triggers: 'create note', 'move note', 'rename note', 'delete note', 'search vault', 'find notes', 'obsidian', 'vault', 'tag search', 'property query', 'frontmatter'. Do NOT use for non-vault markdown files."
compatibility: Requires obsidian-cli on PATH and a running Obsidian instance.
---

# Obsidian CLI

Use `obsidian-cli` for all Obsidian vault operations.
Never use Write, Edit, Bash `mv`, `mkdir`, or filesystem tools to create or move vault files.
`obsidian-cli` uses Obsidian's index, handles backlink updates,
and applies Templater folder rules automatically.

Get command usage via `obsidian-cli <command>` with no arguments,
never via `--help` (unrecognized flag — runs the command instead).

## File Operations

### Create

```bash
obsidian-cli create path="Notes/Note Title.md" content="..."
```

Pass `path=` alone to `create`.
Do not combine `name=` and `path=` when either contains a dot —
obsidian-cli treats the path as a directory and nests the file inside it.

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

Always include the `.md` suffix in `name=`.
Without it, obsidian-cli treats the segment after the last dot as the extension —
renaming to `Ficus carica var. Brown Turkey` produces a file without `.md`.

### Read

```bash
obsidian-cli read path="Notes/Note Title.md"
```

### Delete

```bash
obsidian-cli delete path="Notes/Note Title.md"
```

Always supply `path=` or `file=` to `delete`.
Running `delete` with no arguments silently trashes the active file.

## Searching

Prefer `obsidian-cli` over `find`, `grep`, or `rg` for vault text searches.
It uses Obsidian's index and handles Unicode paths without escaping quirks.

```bash
obsidian-cli search query="phrase" limit=20
obsidian-cli search:context query="phrase"
obsidian-cli search query="phrase" path="Projects"
obsidian-cli search query="phrase" format=json
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

## Anti-Patterns

Never:
- Use Write, Edit, Bash `mv`, `mkdir`, or `echo >` for vault files (bypasses backlinks, templates, index)
- Use `grep`, `find`, or `rg` for vault text search (use `obsidian-cli search`)
- Run `obsidian-cli create` or `obsidian-cli delete` with no arguments (`create` silently creates `Untitled.md`; `delete` silently trashes the active file)
- Use `--help` flag with any subcommand (unrecognized — runs the command instead)
- Omit `.md` suffix in `name=` argument to `rename`
- Combine `name=` and `path=` when either contains a dot in `create`
