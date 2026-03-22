---
name: pi-coding-agent
description: "Guides agents working with the pi coding agent CLI. Use when testing skills with pi, analyzing pi session transcripts, or running pi non-interactively. Covers correct CLI flags, skill testing workflow, and transcript JSONL schema. Triggers: 'test skill with pi', 'pi transcript', 'run pi', 'pi session', 'pi --skill', '--no-skills'. Do NOT use for Claude Code CLI usage or general coding tasks."
compatibility: Requires pi (pi-coding-agent) on PATH.
---

# Pi Coding Agent

Pi is a coding agent CLI with read, bash, edit, write tools
and session management.

This skill covers pi-specific workflows:
testing skills locally, running non-interactive prompts,
and analyzing session transcripts.

## Testing Skills Locally

**Always use `--no-skills --skill <path>` together when testing local edits.**

Without `--no-skills`, pi also discovers installed skills
from `~/.pi/agent/skills/` and may load the stale installed copy
instead of your source edits.

```bash
pi --no-skills --skill ./path/to/skill-dir/ \
  --no-session \
  --model 'anthropic/claude-sonnet-4-20250514' \
  -p "Your test prompt here"
```

Required flags for skill testing:

- `--no-skills` (`-ns`) — disable automatic skill discovery
- `--skill <path>` — load skill from local directory (can repeat for multiple skills)
- `-p` (`--print`) — non-interactive mode: process prompt and exit
- `--no-session` — don't save the test session
- `--session-dir <dir>` — custom directory for session storage (alternative to `--no-session`)
- `--model <provider/id>` — model in `provider/model-id` format (e.g., `anthropic/claude-sonnet-4-20250514`)

Anti-pattern — using `--skill` without `--no-skills`:

```bash
# Wrong: pi discovers installed skills AND loads your local copy
pi --skill ./my-skill/ -p "test prompt"

# Right: only your local copy is loaded
pi --no-skills --skill ./my-skill/ -p "test prompt"
```

## Analyzing Session Transcripts

Pi stores session transcripts as JSONL files at
`~/.pi/agent/sessions/<project-dir-slug>/`.

**Pi's JSONL schema is NOT the same as Claude Code's.**
Do not use Claude Code field names when querying pi transcripts.

### Schema

Each line is a JSON object with a `type` field.
Top-level event types:

| Type | Fields | Purpose |
|------|--------|---------|
| `session` | `id`, `cwd`, `version` | Session metadata |
| `model_change` | `modelId`, `provider` | Model switch event |
| `thinking_level_change` | `thinkingLevel` | Thinking level switch |
| `message` | `message.role`, `message.content[]` | Conversation turn |
| `compaction` | `summary`, `tokensBefore` | Context window summary |

Message roles: `user`, `assistant`, `toolResult` (not `tool_result`).

Content block types inside `message.content[]`:

| Type | Key fields | Notes |
|------|-----------|-------|
| `text` | `text` | Text content |
| `thinking` | `text` | Extended thinking block |
| `toolCall` | `name`, `arguments`, `id` | Tool invocation (not `tool_use`) |

### Common jq Recipes

Count tool calls by name:

```bash
jq -r '
  select(.type == "message")
  | .message.content[]
  | select(.type == "toolCall")
  | .name
' session.jsonl | sort | uniq -c | sort -rn
```

Extract assistant text responses:

```bash
jq -r '
  select(.type == "message" and .message.role == "assistant")
  | .message.content[]
  | select(.type == "text")
  | .text
' session.jsonl
```

Get session metadata:

```bash
jq 'select(.type == "session")' session.jsonl
```

## Common Mistakes

**Using Claude Code transcript schema on pi transcripts:**

Pi uses `toolCall` and `toolResult`, not `tool_use` and `tool_result`.
Pi wraps content in `.message.content[]`, not `.content[]`.

```bash
# Wrong (Claude Code schema)
jq 'select(.type == "assistant") | .content[] | select(.type == "tool_use")'

# Right (pi schema)
jq 'select(.type == "message" and .message.role == "assistant")
  | .message.content[] | select(.type == "toolCall")'
```

**Using `--skill` by name instead of path:**

```bash
# Wrong: treats "coding" as a path, not a skill name
pi --skill coding -p "test"

# Right: pass the directory path
pi --skill ./modules/agentic/skills/coding/ -p "test"
```

**Forgetting `-p` for non-interactive mode:**

```bash
# Wrong: starts interactive session, hangs
pi --no-skills --skill ./my-skill/ "test prompt"

# Right: -p processes prompt and exits
pi --no-skills --skill ./my-skill/ -p "test prompt"
```

**Wrong model format:**

```bash
# Wrong: bare model name
pi --model claude-sonnet-4-20250514

# Right: provider/model-id format
pi --model anthropic/claude-sonnet-4-20250514
```

## References

See [cli-reference.md](references/cli-reference.md) for full CLI options.
