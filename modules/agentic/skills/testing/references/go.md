# Go Testing Patterns

## Test Context (Go 1.24+)

Use `t.Context()` as the base context in all test code.
Never use `context.Background()` or `context.TODO()` in tests,
including pre-existing scaffolding you are extending.

```go
func TestFetchUser(t *testing.T) {
    ctx := t.Context() // canceled when test completes
    user, err := FetchUser(ctx, "42")
    // ...
}
```

When deriving contexts (deadlines, cancellation, values),
use `t.Context()` as the parent:

```go
func TestTimeout(t *testing.T) {
    ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
    defer cancel()
    // ...
}
```

## Test Naming

Every `Test*` function name carries the literal keywords
`Given`, `When`, and `Then` in order —
`TestGiven<context>When<action>Then<outcome>`.
Snake_case sentence-fragment names are insufficient.

- `Given<context>` fixes the precondition (input shape, prior state, fixture).
- `When<action>` names the action under test (function call, method, event).
- `Then<outcome>` states the observable outcome (return value, error, side effect).

DON'T compress the spec into a sentence-fragment label:

```go
// Bad — drops the Given/When/Then keywords:
func TestApplyDiscount_rejects_a_negative_price(t *testing.T) { ... }
func TestApplyDiscount_reduces_price_by_the_given_percent(t *testing.T) { ... }
```

DO use the keywords explicitly:

```go
// Good — Given/When/Then keywords present in order:
func TestGivenNegativePriceWhenApplyDiscountThenReturnsError(t *testing.T) { ... }
func TestGivenValidPriceAndPercentWhenApplyDiscountThenReducesPrice(t *testing.T) { ... }
```

The same pattern carries through subtest names:

```go
func TestApplyDiscount(t *testing.T) {
    t.Run("Given valid price and percent when ApplyDiscount then reduces price", func(t *testing.T) {
        // ...
    })
    t.Run("Given negative price when ApplyDiscount then returns error", func(t *testing.T) {
        // ...
    })
}
```

Full examples:

```go
func TestGivenNegativePriceWhenApplyDiscountThenReturnsError(t *testing.T) {
    _, err := ApplyDiscount(-1, 10)
    if err == nil {
        t.Fatalf("expected error for negative price")
    }
}

func TestGivenPercentOutOfRangeWhenApplyDiscountThenReturnsError(t *testing.T) {
    _, err := ApplyDiscount(100, 150)
    if err == nil {
        t.Fatalf("expected error for percent > 100")
    }
}

func TestGivenValidPriceAndPercentWhenApplyDiscountThenReducesPrice(t *testing.T) {
    got, err := ApplyDiscount(100, 25)
    if err != nil {
        t.Fatalf("unexpected error: %v", err)
    }
    if got != 75 {
        t.Errorf("got %v, want 75", got)
    }
}
```

## Table-Driven Tests

Valid use — testing same property with different inputs:

```go
func TestParseValid(t *testing.T) {
    tests := []struct {
        name  string
        input string
        want  int
    }{
        {"single_digit", "5", 5},
        {"double_digit", "42", 42},
        {"with_whitespace", " 7 ", 7},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Parse(tt.input)
            if got != tt.want {
                t.Errorf("got %v, want %v", got, tt.want)
            }
        })
    }
}
```

Invalid use — conditional logic based on test case:

```go
// wantErr field creates branching logic — use separate test functions instead
tests := []struct {
    name    string
    input   Type
    want    Type
    wantErr error
}{...}
```

## Race Detection

```go
func TestConcurrent(t *testing.T) {
    // Run with: go test -race
    var wg sync.WaitGroup
    counter := NewCounter()

    for i := 0; i < 100; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            counter.Increment()
        }()
    }

    wg.Wait()

    if counter.Value() != 100 {
        t.Errorf("got %d, want 100", counter.Value())
    }
}
```

## Test Helpers

```go
func assertEqual(t *testing.T, got, want interface{}) {
    t.Helper()
    if !reflect.DeepEqual(got, want) {
        t.Fatalf("got %v, want %v", got, want)
    }
}
```

## Build Tags

```go
//go:build integration
// +build integration

package mypackage

func TestDatabaseIntegration(t *testing.T) {
    if testing.Short() {
        t.Skip("skipping in short mode")
    }
    // Integration test
}
```

Run with: `go test -tags=integration`

## Property-Based Testing

```go
func TestSortProperty(t *testing.T) {
    property := func(data []int) bool {
        sorted := make([]int, len(data))
        copy(sorted, data)
        Sort(sorted)

        for i := 1; i < len(sorted); i++ {
            if sorted[i] < sorted[i-1] {
                return false
            }
        }
        return true
    }

    if err := quick.Check(property, nil); err != nil {
        t.Error(err)
    }
}
```

## Benchmarking

```go
func BenchmarkOperation(b *testing.B) {
    data := setupData()
    b.ResetTimer()

    for i := 0; i < b.N; i++ {
        result := Operation(data)
        runtime.KeepAlive(result)
    }
}
```

## Test Fidelity Tier

Real dependency tools:

- `httptest.NewServer` for a real in-process HTTP server
- `t.TempDir()` for a real filesystem under a temp directory (auto-cleaned)
- `//go:build integration` to pair heavyweight tests with a build tag

Generated mocks:

```go
//go:generate go tool moq -out user_mock.go . UserRepository
```

Use `moq`, `mockgen`, or equivalent.
Drive generation via `//go:generate` next to the interface declaration.

Hand-rolled fake anti-pattern:

A hand-written `type fake<Name> struct` with method receivers implementing an
interface, used only to record calls or return canned values, is the
anti-pattern. Replace it with a generated mock.

## Parallel Test Isolation

Mark isolated test functions with `t.Parallel()`:

```go
func TestGivenUserExistsWhenFetchThenReturnsUser(t *testing.T) {
    t.Parallel()
    // each test gets its own server and temp dir
    srv := httptest.NewServer(handler)
    t.Cleanup(srv.Close)
    dir := t.TempDir()
    // ...
}
```

Drop `-p 1` (or equivalent serial-execution config) from the test runner
once tests are properly isolated.

## Test Organization

- Use `package name_test` for black-box testing (default)
- Use `package name` only when testing internals
- Create `export_test.go` to expose internals when needed

## Test Quality

- Race detector: run tests with `go test -race`
- Cleanup: use `defer` or `t.Cleanup()` in all paths
