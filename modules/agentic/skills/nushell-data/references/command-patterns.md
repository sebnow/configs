# Nushell Command Patterns Reference

Extended patterns for less common operations.
Load this file when SKILL.md patterns don't cover the task.

## Group-By with Aggregation

`group-by` creates a record keyed by field values.
`transpose` converts it to a table for further processing.

```bash
# Group, count, and aggregate per group
nu -c 'open data.jsonl | lines | each { from json }
  | where type == "tool_use"
  | group-by name
  | transpose tool entries
  | insert count { |r| $r.entries | length }
  | insert total_tokens { |r| $r.entries | get usage.input_tokens | math sum }
  | reject entries
  | sort-by count --reverse'
```

## String Building from Tables

```bash
# Comma-separated summary
nu -c 'open data.json
  | each { |r| $"($r.tool): ($r.calls)" }
  | str join ", "'

# Multi-line report
nu -c 'open data.json
  | each { |r| $"($r.tool) - ($r.calls) calls, ($r.total_tokens) tokens" }
  | str join (char newline)'
```

## Working with Nested Data

Cell paths access nested fields with dot notation:

```bash
nu -c 'open data.json | get config.database.host'
nu -c 'open data.json | get items.0.name'           # index into lists
```

Flatten nested structures:

```bash
nu -c 'open data.json | flatten'                     # one level
nu -c 'open data.json | flatten --all'               # recursive
```

## Multiple Aggregations

```bash
nu -c 'open data.json | {
  total_calls: ($in | get calls | math sum),
  avg_tokens: ($in | get total_tokens | math avg),
  max_duration: ($in | get avg_duration_ms | math max)
}'
```

## Sorting

```bash
nu -c 'open data.json | sort-by calls'               # ascending
nu -c 'open data.json | sort-by calls --reverse'     # descending
nu -c 'open data.json | sort-by calls --reverse | first 3'  # top N
```

## Deduplication

```bash
nu -c 'open data.json | uniq'                        # exact row match
nu -c 'open data.json | uniq-by name'                # by specific column
```

## Column Operations

```bash
nu -c 'open data.json | select tool calls'           # keep columns
nu -c 'open data.json | reject avg_duration_ms'      # drop columns
nu -c 'open data.json | rename name count'           # rename columns
nu -c 'open data.json
  | insert ratio { |r| $r.total_tokens / $r.calls }' # add column
nu -c 'open data.json
  | update calls { |r| $r.calls + 1 }'               # modify column
```

## Piping from External Commands

When data comes from stdout rather than a file,
use `$in` or `from json`:

```bash
curl -s https://api.example.com/data | nu -c '$in | from json | where active == true'
some-command | nu -c '$in | lines | each { from json } | ...'
```

## Supported Formats

Loading: `open` auto-detects from extension.
Explicit parsing: `from json`, `from csv`, `from yaml`, `from toml`,
`from xml`, `from tsv`, `from ssv`, `from xlsx`, `from ods`, `from nuon`.

Serialization: `to json`, `to csv`, `to yaml`, `to toml`,
`to xml`, `to tsv`, `to md`, `to html`, `to nuon`, `to text`.

## Math Commands

`math sum`, `math avg`, `math min`, `math max`,
`math median`, `math mode`, `math stddev`, `math variance`,
`math product`, `length`.

```bash
nu -c 'open data.json | get calls | math sum'
nu -c 'open data.json | get calls | math avg'
nu -c 'open data.json | get calls | describe'        # type and shape info
nu -c 'open data.json | length'                      # row count
```
