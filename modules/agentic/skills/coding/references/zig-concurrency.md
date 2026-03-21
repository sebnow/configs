# Zig Concurrency (0.16+)

Zig 0.16 replaced the removed `async`/`await` keywords
with `std.Io`-based concurrency.
Do not use `async`, `await`, `suspend`, or `resume` —
they no longer exist.

## std.Io

`std.Io` is a vtable-based interface,
analogous to `std.mem.Allocator` but for I/O.
It abstracts over backends:

| Backend | Model | Status |
|---------|-------|--------|
| `Io.Threaded` | OS thread pool + blocking syscalls | Stable |
| `Io.Evented` | io_uring + userspace fibers | Experimental |
| `Io.Dispatch` | GCD + userspace fibers | Experimental |

Code written against `std.Io` works across all backends
without modification.

## async vs concurrent

"Asynchrony is not concurrency" is an explicit Zig design principle.

- `io.async(fn)` — may execute out of order.
  No parallelism guarantee. Can execute inline on the calling thread.
- `io.concurrent(fn)` — requires parallel execution.
  Dispatches to thread pool (`Threaded`) or fiber (`Evented`/`Dispatch`).

Use `io.async` when operations can overlap but don't need parallelism.
Use `io.concurrent` when operations must run in parallel
(e.g., two independent polling loops).

## Synchronization Primitives

Synchronization primitives live on `Io`, not `std.Thread`.
Use these when the project uses `std.Io`:

| Primitive | Purpose |
|-----------|---------|
| `Io.Mutex` | Mutual exclusion with cancellation support |
| `Io.Condition` | Wait/signal/broadcast |
| `Io.Semaphore` | Wait/post counting |
| `Io.RwLock` | Shared and exclusive locking |
| `Io.Queue(T)` | Thread-safe producer-consumer queue |

All are statically initializable (`.init`).
All are I/O-aware: they support cancellation
and work correctly across all backends.

Prefer `Io.*` primitives over `std.Thread.Mutex` etc.
when using `std.Io` — mixing two concurrency models
adds unnecessary complexity.

## Patterns

### Two concurrent loops sharing state

Use `io.concurrent()` for each loop
and `Io.Queue(T)` for communication:

```zig
const std = @import("std");

fn platformLoop(io: std.Io, input_queue: *std.Io.Queue(Command)) void {
    while (true) {
        const line = readInput(io);
        input_queue.put(.{ .user_message = line });
    }
}

fn applicationLoop(io: std.Io, input_queue: *std.Io.Queue(Command)) void {
    while (true) {
        const cmd = input_queue.get(); // blocks until available
        processCommand(io, cmd);
    }
}

pub fn main(io: std.Io) void {
    var queue: std.Io.Queue(Command) = .init;
    io.concurrent(platformLoop, .{ io, &queue });
    applicationLoop(io, &queue);
}
```

### Prefer io.concurrent over std.Thread

When the project already uses `std.Io`,
prefer `io.concurrent()` over `std.Thread.spawn()`.
This keeps one concurrency model
and works across all `Io` backends.

Use `std.Thread` only when you need an OS thread
outside the `std.Io` executor
(e.g., a thread that must not participate in the I/O backend).

## Verify APIs

Use `zigdoc std.Io` to check the current API surface.
Drill into specifics:
`zigdoc std.Io.Queue`,
`zigdoc std.Io.Mutex`.
