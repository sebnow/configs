# Go Modern APIs

Go evolves across releases.
Use the latest APIs available for the project's Go version.
Do not rely on training data for API knowledge —
verify with `go doc`.

## errors.AsType (Go 1.26+)

`errors.AsType` is a generic replacement for `errors.As`.
Use `errors.AsType` in all Go 1.26+ code
instead of `errors.As` with a pre-declared pointer variable.

```go
// errors.AsType — use this (Go 1.26+)
if pathErr, ok := errors.AsType[*fs.PathError](err); ok {
    fmt.Println("failed at path:", pathErr.Path)
}

// errors.As — do not use in new Go 1.26+ code
var pathErr *fs.PathError
if errors.As(err, &pathErr) {
    fmt.Println("failed at path:", pathErr.Path)
}
```

`errors.AsType` is preferred because it:

- Provides compile-time type safety through generics
- Eliminates pre-declared variables, keeping scope tight
- Avoids reflection, yielding better performance
