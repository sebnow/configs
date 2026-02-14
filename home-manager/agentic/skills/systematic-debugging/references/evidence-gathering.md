# Evidence Gathering Techniques

## Debug Statement Injection

Format: `[DEBUG:location:line] variable_values`

Examples:

```c
fprintf(stderr, "[DEBUG:UserManager::auth:142] user=\"%s\" id=%d result=%d\n", user, id, result);
```

```python
import logging
logger = logging.getLogger(__name__)
logger.debug("[DEBUG:auth_user:142]", extra={"user": user, "id": id, "result": result})
```

```go
import "log/slog"
slog.DebugContext(ctx, "[DEBUG:AuthUser:142]", "user", user, "id", id, "result", result)
```

All debug statements must include "DEBUG:" prefix for easy cleanup.

## Language-Specific Debugging Tools

- Python: pdb, cProfile, tracemalloc, pytest -vv
- Go: delve, pprof, go test -race, go test -v
- C/C++: gdb, lldb, valgrind, -fsanitize=address/undefined, perf
- Rust: rust-gdb, rust-lldb, valgrind, cargo test --nocapture
- JavaScript/Node: node --inspect, Chrome DevTools, --trace-warnings
- System: strace (Linux), dtrace (macOS/BSD), tcpdump, lsof

Verify tool availability before invoking.

## Investigation Strategies by Issue Type

**Memory Issues:**
- Log pointers and content
- Track allocations
- Enable sanitizers (AddressSanitizer, Valgrind)
- Check use-after-free, double-free, buffer overflows

**Concurrency Issues:**
- Log thread/goroutine IDs with state
- Track lock acquisitions and releases
- Enable race detectors
- Look for deadlocks, races, ordering problems

**Performance Issues:**
- Time suspect code sections
- Use profilers before extensive debug statements
- Track allocations and GC pressure
- Identify hot paths

**State/Logic Issues:**
- Log state transitions with old and new values
- Break complex conditions into parts
- Track variable changes through execution
- Verify input validation

**Integration Issues:**
- Log all external interactions (API calls, database queries, file I/O)
- Verify configuration and connections
- Check network issues and timeouts
- Test with minimal dependencies to isolate
