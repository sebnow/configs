# Go Modern APIs

Go evolves across releases.
Use the latest APIs available for the project's Go version.
Do not rely on training data for API knowledge —
verify with `go doc`.

## new() with initial value (Go 1.26+)

`new` accepts an expression argument,
returning a pointer to the initialized value.

```go
// Go 1.26+
p := new(42)
cfg := Config{Timeout: new(5 * time.Second)}

// Before 1.26 — required a temporary variable or helper
age := 42
p := &age
```

Particularly useful for optional pointer fields in struct literals.

## errors.AsType (Go 1.26+)

Generic replacement for `errors.As`.
Use `errors.AsType` in all Go 1.26+ code
instead of `errors.As` with a pre-declared pointer variable.

```go
// Go 1.26+
if pathErr, ok := errors.AsType[*fs.PathError](err); ok {
    fmt.Println("failed at path:", pathErr.Path)
}

// Before 1.26
var pathErr *fs.PathError
if errors.As(err, &pathErr) {
    fmt.Println("failed at path:", pathErr.Path)
}
```

Provides compile-time type safety,
eliminates pre-declared variables,
and avoids reflection.

## sync.WaitGroup.Go (Go 1.25+)

Combines `Add(1)` + goroutine launch + `defer Done()`.

```go
// Go 1.25+
var wg sync.WaitGroup
wg.Go(func() {
    doWork() // Done() called automatically
})
wg.Wait()

// Before 1.25
var wg sync.WaitGroup
wg.Add(1)
go func() {
    defer wg.Done()
    doWork()
}()
wg.Wait()
```

## testing.B.Loop (Go 1.24+)

Replaces manual `b.N` loop in benchmarks.

```go
// Go 1.24+
func BenchmarkFoo(b *testing.B) {
    for b.Loop() {
        doWork()
    }
}

// Before 1.24
func BenchmarkFoo(b *testing.B) {
    for range b.N {
        doWork()
    }
}
```

Runs setup/cleanup once per `-count`,
prevents compiler from optimizing away the loop body.

## testing.T.Context (Go 1.24+)

Returns a context canceled when the test completes.

```go
func TestFoo(t *testing.T) {
    ctx := t.Context() // canceled after test
    result := doWork(ctx)
    // ...
}
```

## testing/synctest (Go 1.25+)

Deterministic testing of concurrent code.
Experimental in 1.24, stable in 1.25.

```go
// Go 1.25+
func TestConcurrent(t *testing.T) {
    synctest.Test(t, func(t *testing.T) {
        // time and goroutines are deterministic here
        synctest.Wait() // waits for all goroutines to block
    })
}
```

## omitzero JSON tag (Go 1.24+)

Omits zero-valued fields in JSON encoding,
including `time.Time` (which `omitempty` does not omit).

```go
type Event struct {
    Time time.Time `json:"time,omitzero"` // omitted when zero
    Name string    `json:"name,omitzero"` // omitted when ""
}
```

`omitempty` does not work for `time.Time`
because a zero `time.Time` is not the empty value of a struct.
Use `omitzero` when zero-value semantics are needed.

## runtime.AddCleanup (Go 1.24+)

Replaces `runtime.SetFinalizer`.
Supports multiple cleanups per object,
works with interior pointers,
avoids cycle-induced leaks.

```go
// Go 1.24+
runtime.AddCleanup(obj, func(data cleanupData) {
    cleanup(data)
}, myData)

// Before 1.24
runtime.SetFinalizer(obj, func(o *MyType) { cleanup(o) })
```

## go.mod tool directive (Go 1.24+)

Replaces the `tools.go` blank-import pattern.

```
// go.mod
tool golang.org/x/tools/cmd/stringer

// Add with:     go get -tool golang.org/x/tools/cmd/stringer
// Run with:     go tool stringer
```

## Iterator functions (Go 1.24+)

`strings` and `bytes` gained iterator-returning variants:

- `Lines(s)` — iterate over newline-terminated lines
- `SplitSeq(s, sep)` — iterate over split substrings
- `SplitAfterSeq(s, sep)` — iterate, keeping separator
- `FieldsSeq(s)` — iterate over whitespace-delimited fields
- `FieldsFuncSeq(s, f)` — iterate with custom split function

```go
for line := range strings.Lines(text) {
    process(line)
}
```

Prefer these over allocating `strings.Split` when iterating.

## reflect.TypeAssert (Go 1.25+)

Generic type assertion from `reflect.Value` without allocation.

```go
// Go 1.25+
val := reflect.TypeAssert[MyType](v)

// Before 1.25 — allocates via interface conversion
val := v.Interface().(MyType)
```

## Crypto: rand parameter ignored (Go 1.26+)

Crypto key generation functions now ignore the `rand io.Reader`
parameter and always use a secure internal source.
Affected functions include `ecdsa.GenerateKey`,
`ecdh.Curve.GenerateKey`, `ed25519.GenerateKey`,
`rsa.GenerateKey`, and others.

Code still compiles but the first argument has no effect.
For deterministic tests,
use `testing/cryptotest.SetGlobalRandom`.

## Deprecations

- `runtime.SetFinalizer` — use `runtime.AddCleanup` (Go 1.24+)
- `runtime.GOROOT()` — use `go env GOROOT`
- `crypto/cipher.NewOFB`, `NewCFBEncrypter`, `NewCFBDecrypter` — use AEAD modes
- `crypto/rsa` PKCS#1 v1.5 encryption — use OAEP (Go 1.26+)
- `net/http/httputil.ReverseProxy.Director` — use `Rewrite` field
