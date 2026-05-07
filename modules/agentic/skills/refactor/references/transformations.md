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

---

## Replace Constant with Method on the Owning Type

### Detection

A package-level `const` (or static field, or top-level variable) identifies a
specific type — its name, its kind, its routing key, its event name — and lives
beside that type as a free-floating declaration. Telltale signs: the constant
and the type appear in pairs (`FooMessageName` + `FooMessage`,
`OrderEventType` + `OrderEvent`); every call site that uses the constant also
mentions the type; renaming one without the other compiles but breaks the
contract silently.

### Smell

The link between the constant and the type is a naming convention readers must
recognize, not a relationship the type system enforces. The two declarations
can drift, callers can use the string literal directly without going through
the type, and adding a new variant requires remembering to add the constant
too. Process-wide configuration values that are not owned by any single type
(queue names, default URLs, feature flags) do not have this smell — leave them
as constants.

### Refactor move

Move the constant onto the type as a method that returns the literal. Update
call sites to obtain the value from the type (`Foo{}.Name()` or
`(*Foo)(nil).Name()`). Delete the free-floating constant. The string is now
unreachable without going through the type, so renames stay coupled and a new
variant cannot omit the identifier.

### Example

Before:

```go
const AppealOutcomeUpdateMessageName = "AppealOutcomeUpdate"

type AppealOutcomeUpdateMessage struct {
    Outcome    AppealOutcome `json:"outcome"`
    ReviewedBy UserReference `json:"reviewedBy"`
}

// used as:
workflow.SetUpdateHandlerWithOptions(ctx, AppealOutcomeUpdateMessageName, ...)
```

After:

```go
type AppealReviewed struct {
    Outcome    AppealOutcome `json:"outcome"`
    ReviewedBy UserReference `json:"reviewedBy"`
}

func (AppealReviewed) UpdateName() string { return "AppealOutcomeUpdate" }

// used as:
workflow.SetUpdateHandlerWithOptions(ctx, AppealReviewed{}.UpdateName(), ...)
```

Counter-example: a `DefaultTaskQueueName` used by every workflow registration
in the package is process-wide configuration, not owned by any single type —
leave it as a `const`.

---

## Move to Own File on Collaboration Friction

### Detection

A single file accumulates many independent units of work — handlers, commands,
event types, route registrations — and the team's history shows it has become a
merge-conflict bottleneck. Telltale signs: the same file appears in conflict
markers across unrelated feature branches; every new endpoint or message type
is appended to the same file by convention; recent merge commits record
"resolved conflicts in X" repeatedly. The units inside share only the receiver
or the package, not domain logic — each one delegates to its own collaborator
and could be read in isolation.

### Smell

The trigger is operational, not architectural. Concurrent edits collide because
unrelated work shares one editing surface, not because the design is wrong.
Line count alone is not the smell: a 2000-line file that one team owns and
edits sequentially has no friction; a 400-line file two teams append to every
day does. Splitting on length, "readability", or "this file is too big" misses
the trigger and invites speculative redesign — new sub-packages, new
abstractions — that the friction does not justify.

### Refactor move

Relocate units verbatim into per-resource (or per-feature) files in the same
package: `server.go` → `server_reports.go`, `server_decisions.go`, etc. Move
methods, not logic — no signature changes, no new types, no new package
boundaries. Receivers, imports, and call sites stay identical. The file that
was contentious becomes thin or disappears; future endpoints land in their own
file by default, so the bottleneck does not re-form. If a deeper redesign is
warranted, do it in a separate move — this one is pure relocation.

### Example

Before — `api/server.go` (every handler in one file, contentious in git):

```go
package api

func (s *Server) FindIssueCategories(ctx context.Context, req *FindIssueCategoriesRequest) (*FindIssueCategoriesResponse, error) { ... }
func (s *Server) BatchGetIssueCategory(ctx context.Context, req *BatchGetIssueCategoryRequest) (*BatchGetIssueCategoryResponse, error) { ... }
func (s *Server) FindReports(ctx context.Context, req *FindReportsRequest) (*FindReportsResponse, error) { ... }
func (s *Server) BatchGetReport(ctx context.Context, req *BatchGetReportRequest) (*BatchGetReportResponse, error) { ... }
// ... fifteen more handlers across unrelated resources
```

After — issue-category handlers extracted verbatim into their own file:

```go
// api/server_issue_categories.go
package api

func (s *Server) FindIssueCategories(ctx context.Context, req *FindIssueCategoriesRequest) (*FindIssueCategoriesResponse, error) { ... }
func (s *Server) BatchGetIssueCategory(ctx context.Context, req *BatchGetIssueCategoryRequest) (*BatchGetIssueCategoryResponse, error) { ... }
```

Counter-example: a 600-line file that one author maintains and that has never
shown up in a conflict resolution does not warrant this move. Files cluster
inside a flat package by name prefix (`api_key.go`, `api_key_activities.go`)
rather than escalating to sub-packages speculatively.
