## Replace Custom Wrapper with Library

### Detection

A struct or interface in this repo wraps a maintained-library type and forwards
every method to it without adding behavior. Telltale signs: a one-method
interface whose signature matches the library type exactly; a `Client`/`Wrapper`
whose methods all delegate to the embedded library value; the wrapper has no
production substitute and no test substitute.

### Smell

The wrapper does not absorb complexity — it re-exports it under a new name.
Callers gain no leverage; readers must learn the wrapper before realizing it is
the library type with extra steps. The "swappability for tests" justification
fails the two-adapters test: only one implementation exists.

### Refactor move

Delete the wrapper. Have callers depend on the library type directly. If a
small amount of behavior was added (e.g. a fixed base URL, a default header),
keep a struct that owns those fields and uses the library type as a concrete
field — but drop the interface and the pass-through methods.

### Example

Before:

```go
type Doer interface {
    Do(*http.Request) (*http.Response, error)
}

type Client struct {
    httpClient Doer
    baseURL    *url.URL
}

func NewClient(httpClient Doer, baseURL *url.URL) *Client {
    return &Client{httpClient: httpClient, baseURL: baseURL}
}
```

After:

```go
type Client struct {
    httpClient *http.Client
    baseURL    *url.URL
}

func NewClient(httpClient *http.Client, baseURL *url.URL) *Client {
    return &Client{httpClient: httpClient, baseURL: baseURL}
}
```
