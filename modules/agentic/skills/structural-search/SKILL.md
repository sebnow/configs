---
name: structural-search
description: "Guides use of ast-grep for structural code search and rewrite via the Bash tool. Use when analyzing code structure, finding patterns across a codebase, refactoring, understanding type hierarchies, or exploring unfamiliar code. Triggers: 'find all implementations of', 'refactor', 'rename', 'find usages', 'code structure', 'type definitions', 'interface implementations', large-scale code changes. Do NOT use for simple text search, log searching, or non-code file search."
compatibility: Requires ast-grep (sg) installed and accessible via PATH
allowed-tools: "Bash(ast-grep run:*) Bash(ast-grep scan:*)"
---

# Structural Search with ast-grep

ast-grep matches code by AST structure, not text.
It returns only matched nodes - no surrounding code, no comments, no noise.
A single ast-grep command replaces Glob + Grep + Read chains
with 80% less output for the same complete result.

Prefer ast-grep over Grep+Read when the question is about code structure
(definitions, call sites, type hierarchies, API surfaces).
Prefer Grep for non-code files, literal strings, or textual patterns.

The output from ast-grep contains the full matched code.
Do not follow up with Read or Grep to see what ast-grep already returned.

## Kind-based Search

Finds all AST nodes of a given type, regardless of syntax variations
(generics, extends, decorators, etc.).

Pass rules as JSON to avoid YAML indentation issues.
Always pipe through `jq -r '.text'` to get full matched content:

```bash
ast-grep scan --inline-rules '{"id":"find","language":"LANG","rule":{"kind":"KIND"}}' --json=stream PATH | jq -r '.text'
```

Common node kinds:

- TypeScript: `interface_declaration`, `class_declaration`, `function_declaration`, `type_alias_declaration`
- Go: `type_declaration`, `function_declaration`, `method_declaration`
- Python: `class_definition`, `function_definition`
- Rust: `impl_item`, `struct_item`, `trait_item`, `function_item`

Compose rules with `inside` (parent context) and `has` (child content):

```bash
# All TypeScript interfaces with full bodies
ast-grep scan --inline-rules '{"id":"find","language":"typescript","rule":{"kind":"interface_declaration"}}' --json=stream src/ | jq -r '.text'

# Only exported interfaces (inside export_statement)
ast-grep scan --inline-rules '{"id":"find","language":"typescript","rule":{"kind":"interface_declaration","inside":{"kind":"export_statement"}}}' --json=stream src/ | jq -r '.text'

# Functions containing a specific call
ast-grep scan --inline-rules '{"id":"find","language":"go","rule":{"kind":"function_declaration","has":{"pattern":"log.Fatal($$$)"}}}' --json=stream . | jq -r '.text'
```

## Pattern Search

Use `run -p` for matching specific code shapes:

```bash
ast-grep run -p 'PATTERN' -l LANG [PATH]
```

- `$NAME` - matches a single AST node
- `$$$` - matches zero or more nodes

Limitation: patterns are syntax-specific.
`export interface $NAME { $$$ }` won't match
interfaces with `extends` or generics.
Use kind-based search when completeness matters.

```bash
# Find all calls to a function
ast-grep run -p 'registerTool($$$)' -l typescript src/

# Method calls on any receiver
ast-grep run -p '$OBJ.addEventListener($$$)' -l typescript

# Go error handling
ast-grep run -p 'if err != nil { $$$ }' -l go
```

## Structural Rewrites

Preview first, then apply. Rewrite commands require user confirmation.

```bash
# Preview
ast-grep run -p 'oldName($$$ARGS)' -l typescript

# Apply (requires confirmation)
ast-grep run -p 'oldName($$$ARGS)' -r 'newName($$$ARGS)' -l typescript -U
```

## Structured Output with jq

```bash
# Extract matched names
ast-grep run -p 'export interface $NAME { $$$ }' -l typescript --json=stream \
  | jq -r '.metaVariables.single.NAME.text'

# Count matches per file
ast-grep run -p 'registerTool($$$)' -l typescript --json=stream \
  | jq -r '.file' | sort | uniq -c | sort -rn
```

## Common Issues

### Pattern misses syntax variants

Cause: `-p` matches literal syntax only.
Solution: Use kind-based search for exhaustive results.

### Go qualified calls (pkg.Func) don't match

Cause: Go's tree-sitter grammar parses `pkg.Func(args)` ambiguously.
Solution: Match the unqualified function name instead.

### Debug a pattern

```bash
ast-grep run -p 'your pattern' -l typescript --debug-query=pattern
```