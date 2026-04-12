---
name: md
description: "Provides the md CLI for querying and mutating Markdown files. Prefer md over Edit for replacing or appending content between structural markers (headings, comments, nodes). Use when replacing sections, appending to sections, mutating frontmatter fields, extracting frontmatter, listing headings, finding links/wikilinks, counting words, extracting code blocks or comments, validating links, finding incoming links, format conversion (YAML/TOML), or batch processing .md files. Triggers: 'replace section', 'append content', 'edit markdown', 'update section', 'frontmatter', 'wikilinks', 'headings', 'markdown metadata', 'word count', 'broken links', 'code blocks', 'comments', 'footnotes', 'incoming links', 'backlinks', '.md files'. Do NOT use for full-text search across files (use grep/rg), non-markdown file formats, or rendering markdown to HTML."
compatibility: Requires md >=0.2.0 on PATH.
---

# Markdown Query and Mutation

Prefer `md` over the Edit tool
when replacing or appending content
at structural boundaries in markdown files
(headings, comment markers, any extractor with `.source`).
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

| Operation | Context | Effect |
| --- | --- | --- |
| `replace(str)` | After any extractor with `.source` | Replaces matched span |
| `append(str)` | After any extractor with `.source` | Inserts after matched span |
| `set(field, val)` | After `frontmatter` | Sets a frontmatter field |
| `del(field)` | After `frontmatter` | Deletes a frontmatter field |
| `.field += [val]` | After `frontmatter` | Appends to a frontmatter array |

`replace()` and `append()` operate on the `.source` span
of any extractor — not just `nodes`.
This means you can target comments, headings, codeblocks, etc. directly.

`md` has no subcommands — the interface is always `md '<program>'`.

### Replacing content between markers

Use `nodes | skip_until() | take_until() | replace()` to target
content between structural boundaries.
This is more robust than Edit's exact string matching
because it works regardless of what the content currently contains.

```bash
# Replace content between Obsidian comment markers
md 'nodes | skip_until(.text == "begin notes") | take_until(.text == "end notes") | replace("New content here.\n\n")' -i note.md

# Replace content under a heading (stops at next h2)
md 'nodes | skip_until(.text == "Notes") | take_until(.type == "heading" and .depth == 2) | replace("Updated.\n\n")' -i note.md

# Append after a section
md 'nodes | skip_until(.text == "Notes") | take_until(.type == "heading") | append("More.\n\n")' -i note.md
```

Both `skip_until` and `take_until` exclude their matched element.
The selected range is the content strictly between the two boundaries.
Use `.depth` in `take_until` to control whether subsections are included.
End `replace()` and `append()` strings with `\n\n`
to preserve the blank line before the next section.

Empty ranges (adjacent markers with no content between them)
are supported — `replace()` and `append()` work correctly.

### Targeting specific elements directly

Any extractor with `.source` supports `replace()` and `append()`.

```bash
# Append content after a specific comment marker
md 'comments | select(.text == "begin notes") | first | append("New note.\n\n")' -i note.md

# Replace a specific heading's text
md 'headings | select(.text == "Old Title") | first | replace("# New Title\n")' -i note.md
```

### Frontmatter mutation

```bash
md 'frontmatter | set(.draft, false)' -i note.md
md 'frontmatter | del(.draft)' -i note.md
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
| Append to a section or after a comment | `md` |
| Mutate frontmatter fields | `md` |
| Insert text at a known exact location | Edit |
| Rename a specific phrase | Edit |

## Extractors

All array extractors include a `.source` field
that identifies the span in the document,
enabling `replace()` and `append()` on any extractor.

| Extractor     | Returns                                                      |
| ------------- | ------------------------------------------------------------ |
| `frontmatter` | YAML/TOML frontmatter as a record                            |
| `body`        | Document body without frontmatter                            |
| `headings`    | `.depth`, `.text`, `.line`, `.source`                        |
| `links`       | `.kind`, `.target`, `.text`, `.line`, `.source`              |
| `stats`       | `.words`, `.lines`                                           |
| `tags`        | `.name`, `.line`, `.source`                                  |
| `codeblocks`  | `.language`, `.content`, `.start_line`, `.end_line`, `.source` |
| `comments`    | `.kind`, `.text`, `.line`, `.source`                         |
| `footnotes`   | `.label`, `.text`, `.line`, `.source`                        |
| `nodes`       | `.type`, `.text`, `.line`, `.source`                         |
| `incoming`    | `.source`, `.kind`, `.line` (requires `--dir`)               |

`nodes` types: `heading`, `paragraph`, `codeblock`, `comment`, `footnote`.
Type-specific fields: `.depth` (heading), `.language` (codeblock),
`.kind` (comment), `.label` (footnote).

```bash
md 'frontmatter | .title' note.md
md 'frontmatter | (.title, .tags)' note.md
```

Comma binds looser than pipe.
Always parenthesize: `(.title, .tags)` not `.title, .tags`.

## Filtering

`select(predicate)` filters arrays.
Predicates: `==`, `!=`, `<`, `>`,
`contains(.field, str)`, `startswith(.field, str)`,
`and`, `or`, `not`.

```bash
md 'headings | select(.depth == 2)' note.md
md 'links | select(.kind == "wikilink")' note.md
md 'links | select(contains(.target, "api"))' note.md
```

List operations: `first`, `last`, `count`, `reverse`, `unique`,
`sort(.field)`, `group(.field)`, `map(.field)`.

Record operations: `keys`, `has("field")`.

```bash
md 'headings | select(.depth == 2) | map(.text)' note.md
md 'links | select(.kind == "wikilink") | count' note.md
md 'tags | map(.name) | unique' note.md
md 'frontmatter | keys' note.md
md 'frontmatter | has("draft")' note.md
```

## Format Conversion

`yaml` and `toml` convert between records and text (bidirectional).

```bash
# Record to YAML text
md 'frontmatter | yaml' note.md

# YAML text to record (parse stdin)
echo 'title: Hello' | md 'body | yaml | .title'
```

## Incoming Links

`incoming` finds all files in `--dir` that link to the input file.
Returns `.source` (file path), `.kind`, `.line`.

```bash
md 'incoming' --dir ./vault/ note.md
md 'incoming | select(.kind == "wikilink")' --dir ./vault/ note.md
md 'incoming | map(.source) | unique' --dir ./vault/ note.md
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
