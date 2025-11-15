# Go Testing Patterns

## Table-Driven Tests

Valid use - testing same property with different inputs:
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

Invalid use - conditional logic based on test case:
```go
// Bad: wantErr field creates branching logic
tests := []struct {
    name    string
    input   Type
    want    Type
    wantErr error  // Red flag
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

        // Property: output must be sorted
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
