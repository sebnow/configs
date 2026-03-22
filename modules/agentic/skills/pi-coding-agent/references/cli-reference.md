# Pi CLI Reference

Full CLI options for pi coding agent (v0.61.1).

## Usage

```
pi [options] [@files...] [messages...]
```

## Commands

| Command | Description |
|---------|-------------|
| `pi install <source> [-l]` | Install extension source |
| `pi remove <source> [-l]` | Remove extension source |
| `pi update [source]` | Update installed extensions |
| `pi list` | List installed extensions |
| `pi config` | Open TUI to enable/disable resources |

## Core Options

| Flag | Short | Description |
|------|-------|-------------|
| `--provider <name>` | | Provider name (default: google) |
| `--model <pattern>` | | Model pattern or ID (`provider/id`, optional `:<thinking>`) |
| `--api-key <key>` | | API key (defaults to env vars) |
| `--system-prompt <text>` | | Override system prompt |
| `--append-system-prompt <text>` | | Append to system prompt |
| `--mode <mode>` | | Output mode: text, json, rpc |
| `--print` | `-p` | Non-interactive: process prompt and exit |
| `--verbose` | | Force verbose startup |
| `--offline` | | Disable startup network operations |
| `--help` | `-h` | Show help |
| `--version` | `-v` | Show version |

## Session Options

| Flag | Short | Description |
|------|-------|-------------|
| `--continue` | `-c` | Continue previous session |
| `--resume` | `-r` | Select a session to resume |
| `--session <path>` | | Use specific session file |
| `--fork <path>` | | Fork session into new session |
| `--session-dir <dir>` | | Session storage directory |
| `--no-session` | | Don't save session (ephemeral) |

## Tool Options

| Flag | Description |
|------|-------------|
| `--no-tools` | Disable all built-in tools |
| `--tools <list>` | Comma-separated tools (default: read,bash,edit,write; also: grep,find,ls) |

## Extension and Skill Options

| Flag | Short | Description |
|------|-------|-------------|
| `--extension` | `-e <path>` | Load extension file (repeatable) |
| `--no-extensions` | `-ne` | Disable extension discovery |
| `--skill <path>` | | Load skill file or directory (repeatable) |
| `--no-skills` | `-ns` | Disable skill discovery and loading |
| `--prompt-template <path>` | | Load prompt template (repeatable) |
| `--no-prompt-templates` | `-np` | Disable prompt template discovery |
| `--theme <path>` | | Load theme (repeatable) |
| `--no-themes` | | Disable theme discovery |

## Model Options

| Flag | Description |
|------|-------------|
| `--models <patterns>` | Comma-separated patterns for Ctrl+P cycling (supports globs) |
| `--thinking <level>` | Thinking level: off, minimal, low, medium, high, xhigh |
| `--list-models [search]` | List available models (with optional fuzzy search) |

## Examples

```bash
# Non-interactive with prompt
pi -p "List all .ts files in src/"

# Include files in initial message
pi @prompt.md @image.png "What color is the sky?"

# Continue previous session
pi --continue "What did we discuss?"

# Use specific model with thinking
pi --model sonnet:high "Solve this complex problem"

# Test a local skill
pi --no-skills --skill ./my-skill/ --no-session -p "test prompt"
```

## Session Storage

Sessions are stored as JSONL files at:
`~/.pi/agent/sessions/<project-dir-slug>/`

Project dir slug replaces `/` with `--` and prepends `--`.
Example: `/home/user/code/project` becomes `--home-user-code-project--`.

## Skill Discovery

Pi discovers skills from:
1. `~/.pi/agent/skills/` (installed skills)
2. Paths specified with `--skill` flag

Use `--no-skills` to disable automatic discovery from `~/.pi/agent/skills/`.
Explicit `--skill` paths still load when `--no-skills` is set.
