## Eliminate Phantom Optionality

### Detection

A value is modelled as optional (`*T`, `Option<T>`, nullable column, sum type with
unused variant) but every read site either dereferences without a real fallback
or unwraps inside a defensive `if x != nil` block whose else-branch is dead.
Common shapes: a `LEFT JOIN` whose right side the domain never omits, a pointer
field always set in the constructor, a nullable column the schema description
says "always populated", a `Result`/`Either` whose error arm is never taken.

### Smell

The optionality is a structural artefact, not a domain fact. The defensive
code exists because the type permits a state the business rules forbid.
Readers must reason about an empty case that cannot happen; bug surface grows
because every new caller has to re-derive that the unwrap is safe.

### Refactor move

Tighten the type at the source so the optionality disappears and the
defensive code is no longer needed: `LEFT JOIN` → `INNER JOIN`, `*T` → `T`,
nullable column → `NOT NULL`, eliminate the unused sum-type variant. Delete
the unwrap block. If a few real call sites still need optionality, keep them
optional and tighten the rest — do not paper over a real domain case.

### Example

Before:

```go
var ruleID *uuid.UUID
var ruleSequence *uint32
rows.Scan(..., &ruleID, &ruleSequence, ...)

// Add the rule if it exists (LEFT JOIN may return NULL for rulesets without rules).
if ruleID != nil {
    rule.ID = *ruleID
    rule.Sequence = *ruleSequence
    // ...
}
```

After (SQL: `LEFT JOIN rule` → `INNER JOIN rule`):

```go
// INNER JOIN guarantees rule fields are non-null.
rows.Scan(..., &rule.ID, &rule.Sequence, ...)
```

---

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
