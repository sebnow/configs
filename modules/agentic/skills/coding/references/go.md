# Go

General Go-specific guidance.
For modern API additions across Go releases,
see [go-modern-apis.md](go-modern-apis.md).

## Code generation with go tool

In Go 1.24+, declare code-generation binaries as tool dependencies in
`go.mod` and invoke them through `go tool`.
Tool versions are locked in `go.mod` / `go.sum`,
which makes generation reproducible
without installing binaries in Dockerfiles or build scripts.

Add a tool dependency:

```sh
go get -tool github.com/matryer/moq@v0.5.3
```

This adds a `tool` directive to `go.mod` and pins the version in `go.sum`.

Invoke generators through `go tool` in `//go:generate` directives:

```go
//go:generate go tool moq -out user_mock.go . User
//go:generate go tool swagger generate spec -o swagger.json
//go:generate go tool mockery --name=Repository
```

Run `go generate ./...` to execute every directive in the module
using the exact pinned tool versions.

When a project still has the legacy `tools.go` blank-import pattern
or relies on `go install` in Dockerfiles,
migrate to the `go.mod` `tool` directive.
Remove the `tools.go` file,
remove `go install` invocations from Dockerfiles and Makefiles,
and update `//go:generate` directives to use `go tool`.

### Tool-version compatibility

Tool releases predate the Go versions that come after them.
Older tool versions can break on newer Go releases.
For example,
`moq` v0.2.7 panics on Go 1.26;
upgrade to v0.5.3 or later.
When bumping the project's Go version,
also bump tool versions and run a `go generate ./...` smoke test.

### CGO and tools

A few tools behave differently depending on how they are built.
`go-swagger`,
when installed standalone via `go install`,
historically required `CGO_ENABLED=0` to produce a static binary.
When the same package runs as `go tool swagger`,
the build settings are inherited from the module
and the standalone CGO workaround does not apply.
Drop the `CGO_ENABLED=0` shim from build scripts
once the tool moves under `go tool`.

## go test parallelism

`go test ./...` runs package tests in parallel up to `-p`
(defaults to `GOMAXPROCS`).
Within a package,
individual tests parallelize when they call `t.Parallel()`.

The package-level scheduler adds one constraint:
a package's tests do not run in parallel with tests from its transitive
dependencies.
This is by design.
Shared code is often exercised concurrently across packages,
and serializing dependent packages keeps the race detector's reports
accurate.
Tests for `pkg/a` and `pkg/b` run together if neither depends on the other;
tests for `pkg/a` and `pkg/a/internal` serialize.

The visible symptom is a test run that looks sequential
even with a high `-p` and no `-p 1` set in `GOFLAGS` or a `Makefile`.
This is the scheduler doing its job,
not a configuration problem.

Trace the dependency graph to understand the order:

```sh
go list -deps ./path/to/pkg
```

The output lists every package that `./path/to/pkg` imports transitively.
Any of those packages whose tests are also being run
will serialize against this one.

When a slow test suite is dominated by serialization,
the lever is dependency shape:
splitting a foundational package's tests into a separate package
(or moving heavy fixtures into a leaf package)
reduces the chains the scheduler has to honor.
