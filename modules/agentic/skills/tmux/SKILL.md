---
name: tmux
description: "Teaches agents to control tmux sessions for interactive CLIs and long-running processes via the Bash tool. Use when running REPLs, debuggers, database shells, long-running servers, or any tool requiring a persistent TTY. Triggers: 'start a REPL', 'run interactively', 'interactive session', 'long-running process', 'background server', 'keep running'. Do NOT use for simple one-shot commands or non-interactive background tasks that the Bash tool handles natively."
compatibility: Requires tmux on PATH. Supported on Linux and macOS.
---

# tmux for Agents

The Bash tool runs commands synchronously with a timeout
and cannot interact with programs that require a persistent TTY.
tmux provides programmatic session control:
create detached sessions, send keystrokes, and capture output,
all through non-interactive Bash commands.

Use tmux when you need to:

- Run interactive programs (REPLs, debuggers, database shells)
- Monitor long-running processes (servers, builds, watchers)
- Send input to a running process over time

Do not use tmux for simple one-shot commands.
The Bash tool handles those directly.

## Session Setup

### Socket Isolation

Never use the default tmux server.
Always use a dedicated socket to avoid interfering with user sessions.

```bash
TMUX_TMPDIR=$(mktemp -d)
TMUX="tmux -L agent"
```

Set `TMUX_TMPDIR` before any tmux command
so the socket file is created in an isolated directory.
Use `TMUX_TMPDIR` and `-L agent` consistently in every command.

### Creating Sessions

```bash
TMUX_TMPDIR=$(mktemp -d) $TMUX new-session -d -s myrepl -x 200 -y 50
```

Required flags:

- `-d` - detached (never omit; agents cannot attach)
- `-s <name>` - named session (required for all subsequent commands)
- `-x 200 -y 50` - explicit window size (prevents default 80x24 truncation)

Name sessions descriptively: `python-repl`, `dev-server`, `debug-gdb`.

### Listing Sessions

```bash
$TMUX list-sessions
```

Check for existing sessions before creating new ones
to avoid duplicates.

## Sending Input

### Literal Text

Use `-l` (literal) mode to prevent key-name interpretation:

```bash
$TMUX send-keys -t myrepl -l 'print("hello world")'
$TMUX send-keys -t myrepl Enter
```

Always send `Enter` as a separate command after literal text.
Without `-l`, tmux interprets strings like `C-c` as control sequences.

### Control Keys

Send control keys without `-l`:

```bash
$TMUX send-keys -t myrepl C-c      # interrupt
$TMUX send-keys -t myrepl C-d      # EOF
$TMUX send-keys -t myrepl C-l      # clear screen
```

### Multi-Line Input

For multi-line input,
send each line separately with `Enter` between them:

```bash
$TMUX send-keys -t myrepl -l 'def greet(name):'
$TMUX send-keys -t myrepl Enter
$TMUX send-keys -t myrepl -l '    return f"Hello, {name}"'
$TMUX send-keys -t myrepl Enter
$TMUX send-keys -t myrepl Enter
```

Never paste large blocks at once.
REPLs may misinterpret rapid multi-line input.

## Reading Output

Capture the current pane content:

```bash
$TMUX capture-pane -t myrepl -p -S -50
```

Flags:

- `-p` - print to stdout (required; without it, output goes to a paste buffer)
- `-S -50` - start capture 50 lines back from cursor (negative = history)
- `-J` - join wrapped lines (add when output has long lines)

To capture the entire scrollback:

```bash
$TMUX capture-pane -t myrepl -p -S -
```

Always limit with `-S -N` when you only need recent output.
Full scrollback can be very large.

## Synchronization

After sending a command,
wait for the program to produce output or return to a prompt.
Poll with a timeout:

```bash
for i in $(seq 1 30); do
  output=$($TMUX capture-pane -t myrepl -p -S -5)
  if echo "$output" | grep -q '>>> \|In \['; then
    break
  fi
  sleep 1
done
```

Adapt the `grep` pattern to match the expected prompt:

| Program   | Prompt pattern        |
| --------- | --------------------- |
| Python    | `>>> \|\\.\\.\\. `    |
| IPython   | `In \\[`              |
| Node.js   | `> `                  |
| PostgreSQL| `=# \|=> `            |
| GDB       | `(gdb) `              |
| Shell     | `\\$ `                |

For long-running processes without a prompt (servers, builds),
poll for specific output strings:

```bash
for i in $(seq 1 60); do
  output=$($TMUX capture-pane -t myrepl -p -S -10)
  if echo "$output" | grep -q 'Listening on port'; then
    break
  fi
  sleep 2
done
```

Always set a finite upper bound on poll iterations.
Never poll indefinitely.

## User Notification

After creating any tmux session,
always print the monitoring commands so the user can observe:

```
Session 'dev-server' started. Monitor with:
  TMUX_TMPDIR=/tmp/tmp.xxxx tmux -L agent attach -t dev-server
```

Include the full command with `TMUX_TMPDIR` path,
since the user needs it to connect to the isolated socket.

## Cleanup

Kill sessions when they are no longer needed:

```bash
$TMUX kill-session -t myrepl
```

Kill the entire agent server when all work is done:

```bash
$TMUX kill-server
```

Clean up the socket directory:

```bash
rm -rf "$TMUX_TMPDIR"
```

Cleanup obligations:

- Kill sessions before creating replacements
- Kill the server when all sessions are done
- Always clean up on task completion, even if errors occurred
- Never leave orphaned sessions running

## Common Issues

**Python REPL needs extra Enter for blocks:**
After indented blocks (def, if, for),
send an extra empty `Enter` to execute the block.

**Stale output after send-keys:**
Always poll or sleep before capture-pane.
send-keys returns immediately;
the program needs time to process input.

**Socket not found errors:**
Verify `TMUX_TMPDIR` is set to the same directory
used during session creation.
The socket lives at `$TMUX_TMPDIR/agent`.

**Session name collisions:**
Use `list-sessions` to check before creating.
Kill stale sessions rather than using duplicate names.

**Output truncated at 80 columns:**
Set explicit window dimensions with `-x 200 -y 50`
when creating the session.

## Examples

### Python REPL

```bash
TMUX_TMPDIR=$(mktemp -d) tmux -L agent new-session -d -s python -x 200 -y 50 python3

# Wait for prompt
for i in $(seq 1 15); do
  output=$(TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent capture-pane -t python -p -S -3)
  if echo "$output" | grep -q '>>>'; then break; fi
  sleep 1
done

# Send a command
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent send-keys -t python -l 'import sys; print(sys.version)'
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent send-keys -t python Enter

sleep 1
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent capture-pane -t python -p -S -5
```

### Long-Running Server

```bash
TMUX_TMPDIR=$(mktemp -d) tmux -L agent new-session -d -s server -x 200 -y 50 'npm run dev'

# Wait for server ready
for i in $(seq 1 30); do
  output=$(TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent capture-pane -t server -p -S -10)
  if echo "$output" | grep -q 'ready\|listening\|started'; then break; fi
  sleep 2
done

echo "Session 'server' started. Monitor with:"
echo "  TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent attach -t server"
```

### Debugger Session

```bash
TMUX_TMPDIR=$(mktemp -d) tmux -L agent new-session -d -s gdb -x 200 -y 50 'gdb ./myprogram'

# Wait for GDB prompt
for i in $(seq 1 15); do
  output=$(TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent capture-pane -t gdb -p -S -3)
  if echo "$output" | grep -q '(gdb)'; then break; fi
  sleep 1
done

# Set breakpoint and run
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent send-keys -t gdb -l 'break main'
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent send-keys -t gdb Enter
sleep 1
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent send-keys -t gdb -l 'run'
TMUX_TMPDIR=$TMUX_TMPDIR tmux -L agent send-keys -t gdb Enter
```
