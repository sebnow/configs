---
name: nushell-data
description: "Teaches agents to use nushell for structured data manipulation instead of python3 boilerplate or complex jq pipelines. Use when parsing JSONL, filtering JSON, aggregating data, or converting between formats (JSON, CSV, YAML, TOML). Triggers: JSONL parsing, data aggregation, format conversion, 'count occurrences', 'filter records', 'convert to CSV'. Do NOT use for text search across files (use grep/rg), simple key extraction from a single JSON object (use jq), or tasks requiring libraries beyond nushell builtins."
compatibility: Requires nu (nushell) on PATH.
---

# Nushell for Data Manipulation

Use `nu -c '...'` instead of python3 scripts or complex jq pipelines
for structured data tasks.
Nushell treats all data as structured tables, records, and lists --
no JSON parsing boilerplate needed.

## JSONL Processing

JSONL files (one JSON object per line) are the most common data format
in agent transcripts and logs.

```bash
# Parse JSONL and work with it as a table
nu -c 'open file.jsonl | lines | each { from json } | ...'
```

The `open ... | lines | each { from json }` pattern
converts JSONL into a nushell table.
From there, use standard table operations.

### Count occurrences by field

```bash
nu -c 'open data.jsonl | lines | each { from json }
  | where type == "tool_use" | get name
  | uniq --count | sort-by count --reverse'
```

### Filter and select fields

```bash
nu -c 'open data.jsonl | lines | each { from json }
  | where type == "tool_use" | select name content'
```

### Aggregate numeric fields

```bash
nu -c 'open data.jsonl | lines | each { from json }
  | where type == "tool_use" | get usage.input_tokens | math sum'
```

## Format Conversion

Nushell's `open`, `to`, and `save` commands handle format conversion
without import statements or serialization gymnastics.

```bash
# JSON to CSV
nu -c 'open data.json | to csv | save data.csv'

# CSV to JSON
nu -c 'open data.csv | to json | save data.json'

# YAML to TOML
nu -c 'open config.yaml | to toml | save config.toml'
```

`open` auto-detects format from file extension.
`to csv`, `to json`, `to yaml`, `to toml` handle serialization.
`save` writes to file.

### Convert with filtering

```bash
nu -c 'open data.json | where calls > 10 | select tool calls | to csv | save filtered.csv'
```

## Filtering and Aggregation

`where` filters rows. `get` extracts columns.
Math commands (`math avg`, `math sum`, `math min`, `math max`) aggregate.

```bash
# Filter rows by condition
nu -c 'open data.json | where calls > 10'

# Compute derived columns with insert
nu -c 'open data.json | where calls > 10
  | insert avg_tokens { |r| $r.total_tokens / $r.calls }
  | select tool calls avg_tokens'

# Aggregate a column
nu -c 'open data.json | get total_tokens | math avg'
```

## When NOT to Use Nushell

Use nushell for: multi-record data, format conversion, aggregation.
Do NOT use nushell for these tasks:

**Text search across files** -- use grep/rg.
Nushell's `glob | each { open | lines | where }` is slower and far more verbose.

```bash
# WRONG
nu -c 'glob **/*.nix | each { |f| open $f | lines | where {|l| $l =~ "pattern"} } | flatten'
# RIGHT
rg "pattern" --glob '*.nix'
```

**Single field extraction from one JSON object** -- use jq.
When you have one object and need one field, jq is shorter.

```bash
# WRONG
echo '{"a":{"b":"c"}}' | nu -c '$in | from json | get a.b'
# RIGHT
echo '{"a":{"b":"c"}}' | jq -r '.a.b'
```

**Binary data or file downloads** -- use curl.
Nushell's HTTP commands cannot save binary files directly.

**Tasks requiring external libraries** -- use python3.
Statistical analysis, ML, or anything needing pandas/numpy.

## Anti-Patterns

Never write python3 boilerplate for JSONL parsing:

```python
# WRONG -- wasteful boilerplate
import json
with open("file.jsonl") as f:
    for line in f:
        obj = json.loads(line)
        if obj.get("type") == "tool_use":
            ...
```

Never chain jq with sort/uniq for counting:

```bash
# WRONG -- unnecessary shell pipeline
jq -r 'select(.type == "tool_use") | .name' file.jsonl | sort | uniq -c | sort -rn
```

Both are replaced by a single nushell pipeline.

## Syntax Gotchas

Nushell is not POSIX sh. These are the most common mistakes:

**`save` requires `-f` to overwrite existing files.**
Without it, nushell errors on existing files.

```bash
nu -c '... | save -f output.csv'    # overwrites
nu -c '... | save output.csv'       # errors if file exists
```

**JSONL is not auto-detected by `open`.**
Always use `open file.jsonl | lines | each { from json }`.
Do not use `open file.jsonl` alone -- it reads the file as plain text.

**No `>` redirection.** Use `save` instead.

```bash
# WRONG
nu -c 'open data.json | to csv' > out.csv
# RIGHT
nu -c 'open data.json | to csv | save -f out.csv'
```

**String interpolation uses `$"..."`** with parenthesized expressions.

```bash
nu -c '$"Name: ($row.name), Count: ($row.count)"'
```

**Closure parameters use `{ |param| ... }` syntax.**

```bash
nu -c '... | each { |row| $row.field }'
nu -c '... | insert new_col { |r| $r.a / $r.b }'
```

**`sort-by` not `sort_by`.** Nushell uses hyphens, not underscores.
Same for: `group-by`, `str join`, `math avg`, `split row`.

## Extended Patterns

See [command-patterns.md](references/command-patterns.md)
for group-by with aggregation, string building, nested data access,
column operations, piping from external commands, and full format lists.
