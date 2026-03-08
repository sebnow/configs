---
name: md
description: "Provides the md CLI for querying and mutating Markdown files. Prefer md over Edit for replacing or appending content between structural markers (headings, comments, nodes). Use when replacing sections, appending to sections, mutating frontmatter fields, extracting frontmatter, listing headings, finding links/wikilinks, counting words, extracting code blocks or comments, validating links, or batch processing .md files. Triggers: 'replace section', 'append content', 'edit markdown', 'update section', 'frontmatter', 'wikilinks', 'headings', 'markdown metadata', 'word count', 'broken links', 'code blocks', 'comments', 'footnotes', '.md files'. Do NOT use for full-text search across files (use grep/rg), non-markdown file formats, or rendering markdown to HTML."
compatibility: Requires md on PATH.
---

# Markdown Query and Mutation

Prefer `md` over the Edit tool
when replacing or appending content
at structural boundaries in markdown files
(headings, comment markers, node types).
`md` targets structure, not exact text,
so mutations are robust against content changes.

Also use `md` instead of full-file Read, regex Grep,
or python3 for structured markdown extraction.

## Core Syntax

```bash
md '<program>' [--json] [--dir <path>] [-i] [file]
```

Programs are pipelines: `extractor | filter | operation`.
Without a file argument, reads from stdin.

## Mutation

Mutation programs produce the modified document.
Use `-i` to write changes back to the file.

### Replacing content between markers

Use `nodes | skip_until() | take_until() | replace()` to target
content between structural boundaries.
This is more robust than Edit's exact string matching
because it works regardless of what the content currently contains.

```bash
# Replace content between Obsidian comment markers
md 'nodes | skip_until(.text == "begin notes") | take_until(.text == "end notes") | replace("New content here.\n")' -i note.md

# Replace content under a heading (stops at next h2)
md 'nodes | skip_until(.text == "Notes") | take_until(.type == "heading" and .depth == 2) | replace("Updated.\n")' -i note.md

# Append after a section
md 'nodes | skip_until(.text == "Notes") | take_until(.type == "heading") | append("More.\n")' -i note.md
```

`skip_until` excludes the matched element.
Use `.depth` in `take_until` to control whether subsections are included.

### Frontmatter mutation

```bash
md 'frontmatter | set(.draft, false)' -i note.md
md 'frontmatter | del(.tags)' -i note.md
md 'frontmatter | .tags += ["new-tag"]' -i note.md
```

### Whole-body replacement

```bash
md 'body | replace("# Fresh Start\n\nNew content.\n")' -i note.md
```

## When to Use md vs Edit

| Situation | Tool |
| --- | --- |
| Replace content between markers or headings | `md` |
| Append to a section | `md` |
| Mutate frontmatter fields | `md` |
| Insert text at a known exact location | Edit |
| Rename a specific phrase | Edit |

## Extractors

| Extractor     | Returns                                          |
| ------------- | ------------------------------------------------ |
| `frontmatter` | YAML/TOML frontmatter as a record                |
| `body`        | Document body without frontmatter                |
| `headings`    | Headings with `.depth`, `.text`, `.line`          |
| `links`       | Links with `.kind`, `.target`, `.text`, `.line`   |
| `stats`       | `.words` and `.lines` counts                      |
| `tags`        | Inline tags with `.name`, `.line`                 |
| `codeblocks`  | Code blocks with `.language`, `.content`, `.line` |
| `comments`    | HTML/Obsidian comments with `.kind`, `.text`      |
| `footnotes`   | Footnote definitions with `.label`, `.text`       |
| `nodes`       | Block-level nodes with `.type`, `.text`, `.line`  |

```bash
md 'frontmatter | .title' note.md
md 'frontmatter | (.title, .tags)' note.md
```

## Filtering

`select(predicate)` filters arrays.
Predicates: `==`, `!=`, `<`, `>`, `contains()`, `startswith()`,
`and`, `or`, `not`.

```bash
md 'headings | select(.depth == 2)' note.md
md 'links | select(.kind == "wikilink")' note.md
```

List operations: `first`, `last`, `count`, `reverse`, `unique`,
`sort(.field)`, `group(.field)`, `map(.field)`.

```bash
md 'headings | select(.depth == 2) | map(.text)' note.md
md 'links | select(.kind == "wikilink") | count' note.md
md 'tags | map(.name) | unique' note.md
```

## Link Validation

`exists` adds `.exists` boolean to link records.
`resolve` adds `.path` with resolved filesystem path.
Both require `--dir`.

```bash
md 'links | exists | select(.exists == false)' --dir ./vault/ note.md
```

## Batch Processing

Use `find -exec` for multiple files.

```bash
find vault/ -name '*.md' -exec md 'frontmatter | .title' {} \;
find vault/ -name '*.md' -exec md 'frontmatter | set(.processed, true)' -i {} \;
```

## When NOT to Use md

- **Full-text search across files** — use Grep/rg
- **Non-markdown files** — `md` only parses markdown
- **Inserting at a known exact text location** — use Edit

## JSON Output

Use `--json` for structured output suitable for piping to `jq`.

```bash
md 'headings' --json note.md
md 'links | select(.kind == "wikilink")' --json note.md | jq '.[].target'
```
