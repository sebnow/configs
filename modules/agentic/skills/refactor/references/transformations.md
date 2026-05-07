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

---

## Aggressive Dead-Code Removal After Successor Lands

### Detection

A module, workflow, function, or activity has a named successor that has
already taken over every caller. Telltale signs: a recent commit pair that
introduced the new version and migrated callers; the legacy symbol is
referenced only by its own definition and by registration boilerplate (e.g.
a worker, router, or DI graph); a "kept just in case" comment past its
stated cutover; helpers or activities that exist solely to support the old
path. Temporal / job-queue equivalents: zero in-flight executions older
than the cutover.

### Smell

The migration is complete but the deletion is missing. Readers must
re-derive that the legacy code is unused; new contributors copy patterns
from it because it looks current; the registry still pays the cost of
loading and reasoning about both versions. Leaving dead code "for now"
turns a clean introduce → migrate sequence into a permanent two-version
state, and the cost of removal grows as new code accretes around the old
shape.

### Refactor move

Apply the introduce → migrate → delete sequence and finish the third step:
delete the legacy module, its exclusive helpers and activities, and its
registration in the same commit. Make that commit on its own — never mixed
with hand-written changes elsewhere — so the diff is reviewable as pure
removal and so the bisect history stays clean. The commit message states
which successor took over and why the old version is gone, not just that
it is.

### Example

Before — both workflows registered, only the new one called:

```go
// worker.go
func RegisterWorkflows(w worker.Worker) {
    w.RegisterWorkflow(ProcessDecisionWorkflow) // current; every caller uses this
    w.RegisterActivity(ApplyDecisionAndAudit)

    // Legacy. No callers; kept "just in case" past the cutover.
    w.RegisterWorkflow(ApplyDecisionWorkflow)
    w.RegisterActivity(ApplyDecision)
    w.RegisterActivity(EmitLegacyAudit)
}
```

After — own commit, removal only:

```
refactor: Remove ApplyDecisionWorkflow   (-353 lines)

ProcessDecisionWorkflow has taken over every caller (cutover landed in
2025-W37). Zero in-flight executions remain. Drop the legacy workflow
and its exclusive activities so the worker registry is single-version.
```

```go
// worker.go
func RegisterWorkflows(w worker.Worker) {
    w.RegisterWorkflow(ProcessDecisionWorkflow)
    w.RegisterActivity(ApplyDecisionAndAudit)
}
```

Counter-example: a `TODO` comment that records a known limitation
(`// TODO: derive ScheduleToCloseTimeout from the gRPC deadline`) is not
dead code — it documents a gap, not a superseded path. Leave it.

---

## Promote Primitive to Named Type

### Detection

Multiple semantically distinct values flow through public boundaries as the
same primitive type — typically `string` or `int`. Telltale signs: function
signatures like `func Block(userID, contentID, reporterID string)` where
transposing arguments compiles cleanly; validation rules ("must be non-empty",
"must match prefix `v1_`") re-derived inside every function that accepts the
value; a struct field whose type tells you nothing about its meaning
(`Owner string` vs. `Owner UserReference`).

### Smell

The compiler cannot tell two semantic kinds apart, so call-site transposition
is a runtime bug rather than a type error. Validation rules drift across call
sites because there is no canonical place to put them. New readers must trace
back through the codebase to learn which `string` carries which constraint.
Genuinely opaque values that have no domain semantics — correlation IDs,
free-form labels, log messages — are not affected: they have nothing to put
in a parse constructor and nothing to confuse with another kind.

### Refactor move

Give each kind its own named type with a parse constructor
(`type UserReference string` + `ParseUserReference(s string) (UserReference, error)`).
Make the constructor the only way to obtain the type from a raw string at
boundaries (HTTP, RPC, database). Update signatures to take the named types
so transposition becomes a compile error. Validation that previously lived
inline collapses into the constructor; if there is no validation today, the
empty constructor is still worth introducing for the type-safety alone, and
rules can be added later without changing call sites.

### Example

Before:

```go
func BlockUser(ctx context.Context, userID, contentID string, reporterID string) error {
    if userID == "" { return errors.New("userID required") }
    if contentID == "" { return errors.New("contentID required") }
    if reporterID == "" { return errors.New("reporterID required") }
    return persistBlock(ctx, userID, contentID, reporterID)
}

// Compiles, but routes the action against the wrong subject:
BlockUser(ctx, contentID, userID, reporterID)
```

After:

```go
type UserReference string
type ContentReference string

func ParseUserReference(s string) (UserReference, error) {
    if s == "" { return "", errors.New("user reference required") }
    return UserReference(s), nil
}

func ParseContentReference(s string) (ContentReference, error) {
    if s == "" { return "", errors.New("content reference required") }
    return ContentReference(s), nil
}

func BlockUser(ctx context.Context, user UserReference, content ContentReference, reporter UserReference) error {
    return persistBlock(ctx, user, content, reporter)
}

// Will not compile — ContentReference is not a UserReference:
BlockUser(ctx, content, user, reporter)
```

Counter-example: a `correlationID` carried through request context for tracing
has no domain semantics — no parse rule, no equivalence beyond string equality,
no risk of confusion with another kind. Leave it as `string`.

---

## Replace Opaque Payload with Schema

### Detection

A value flowing through public boundaries — an event payload, a workflow
input, an HTTP body, a configuration record — is carried as an untyped bag:
`map[string]any`, `json.RawMessage`, `interface{}`, a JSON column read as a
string, a dict in a dynamic language. Producers fill the bag with keys and
consumers retrieve them with type assertions or string-keyed lookups.
Telltale signs: every consumer re-derives the same schema with assertions
like `payload["orderId"].(string)`; field-presence and format rules
(`if v == ""`, "must be ISO-4217") repeat across consumers because no type
owns them; a key typo or a producer/consumer naming drift
(`amount_cents` vs. `amountCents`) compiles cleanly and fails silently at
runtime; the documentation for the field is "see the consumers".

### Smell

The schema exists — every reader and writer agrees on it — but lives only
in convention. The type system carries none of it, so the compiler cannot
catch typos, missing fields, or wrong-typed fields, and validation drifts
because there is no canonical place to put it. Every new consumer pays the
re-derivation tax; every new producer can introduce a subtle mismatch
without breaking the build. Genuinely free-form values that have no fixed
schema and no typed consumer — debugging breadcrumbs in a structured log
entry, ad-hoc feature-flag context, opaque vendor responses passed through
to a human reader — are not affected: there is nothing to put in a typed
struct because there is no agreed shape.

### Refactor move

Define a typed schema for the payload and replace the bag with it. In Go:
a struct with named fields, JSON tags where the wire format matters, and a
parse constructor that validates invariants once
(`ParseOrderPlaced(raw []byte) (OrderPlaced, error)`). Producers construct
the typed value at the boundary; consumers receive it already validated.
Field renames now require a compile fix, validation rules collapse into the
constructor, and producer/consumer drift becomes a build error. If the
boundary is a generic envelope (a bus that carries many event kinds), keep
the envelope generic and dispatch to typed payloads per kind — do not
collapse the envelope itself.

### Example

Before:

```go
type Event struct {
    Kind    string
    Payload map[string]any
}

// Every consumer re-derives the schema; producers may drift on key names.
func HandleOrderPlaced(ctx context.Context, e Event) error {
    orderID, ok := e.Payload["orderId"].(string)
    if !ok || orderID == "" { return errors.New("orderId required") }
    amountCents, ok := e.Payload["amountCents"].(float64)
    if !ok || amountCents <= 0 { return errors.New("amountCents must be positive") }
    currency, ok := e.Payload["currency"].(string)
    if !ok || len(currency) != 3 { return errors.New("currency must be ISO-4217") }
    // ...
}
```

After:

```go
type OrderPlaced struct {
    OrderID     string `json:"orderId"`
    AmountCents int64  `json:"amountCents"`
    Currency    string `json:"currency"`
}

func ParseOrderPlaced(raw []byte) (OrderPlaced, error) {
    var p OrderPlaced
    if err := json.Unmarshal(raw, &p); err != nil { return OrderPlaced{}, err }
    if p.OrderID == "" { return OrderPlaced{}, errors.New("orderId required") }
    if p.AmountCents <= 0 { return OrderPlaced{}, errors.New("amountCents must be positive") }
    if len(p.Currency) != 3 { return OrderPlaced{}, errors.New("currency must be ISO-4217") }
    return p, nil
}

// Consumers receive a validated value; producers cannot drift the field names.
func HandleOrderPlaced(ctx context.Context, p OrderPlaced) error { /* ... */ }
```

Counter-example: a structured-log `Details map[string]any` field carrying
arbitrary debugging context — evaluated feature flags, request headers,
ad-hoc breadcrumbs — has no fixed schema, no typed consumer, and no domain
invariants to enforce. Leave it as `map[string]any`.

---

## Extract by Responsibility

### Detection

A single function, method, or type packs several independent jobs into one
body. Each block leans on its own collaborator (a different repository, a
different external client, a different bus or mailer), reacts to its own
failure modes, and changes for its own reason — billing rules change the
pricing block, the schema team changes the inserts, marketing changes the
email copy. Telltale signs: section-comment dividers (`// --- validation ---`,
`// --- persistence ---`); collaborators injected purely to serve one block;
unit tests that have to fake every collaborator just to exercise one slice.

### Smell

The bundling is co-location, not cohesion. Each block has a name a reader
could give to a separate type or function, but lives inside the same body
because the entry point happened to land there. Every change to one block
risks the others; every reader has to load the union of all of them just
to follow one. Length alone is not the smell — a long function that does
one nameable job has nothing to extract; the smell is multiple nameable
jobs sharing one body.

### Refactor move

Give each responsibility its own type or function with a name that says
what it does (`SubmitValidator`, `Pricer`, `OrderRepository`,
`OrderNotifier`). The original function becomes a thin orchestrator that
sequences them — receive input, call validator, call pricer, call
repository, call notifier, return the result. Move signatures, not logic;
each extracted piece keeps the body it had. If a piece resists naming —
"this part doesn't have a noun" — leave it inside; the move applies only
where the name is obvious.

### Example

Before:

```go
func (p *OrderProcessor) Submit(ctx context.Context, req SubmitRequest) (Order, error) {
    // --- validation ---
    if req.CustomerID == "" { return Order{}, errors.New("customerID required") }
    if !strings.Contains(req.CustomerEmail, "@") { return Order{}, errors.New("bad email") }
    for i, item := range req.Items {
        if item.Quantity <= 0 { return Order{}, fmt.Errorf("items[%d].quantity", i) }
    }

    // --- pricing + tax ---
    var subtotal int64
    for _, item := range req.Items {
        unit, _, err := p.rates.LookupUnitPrice(ctx, item.SKU)
        if err != nil { return Order{}, err }
        subtotal += unit * int64(item.Quantity)
    }
    tax, err := p.rates.LookupTax(ctx, req.Region, subtotal)
    // ...

    // --- persistence ---
    tx, _ := p.db.BeginTx(ctx)
    // INSERT orders, INSERT order_items, COMMIT ...

    // --- notification ---
    _ = p.bus.Publish(ctx, "order.placed", order)
    _ = p.smtp.Send(ctx, req.CustomerEmail, "Order confirmed", body)
    return order, nil
}
```

After:

```go
type SubmitValidator struct{}
func (SubmitValidator) Check(req SubmitRequest) error { /* ... */ }

type Pricer struct{ rates RateClient }
func (p Pricer) Quote(ctx context.Context, req SubmitRequest) (Quote, error) { /* ... */ }

type OrderRepository struct{ db DB }
func (r OrderRepository) Insert(ctx context.Context, q Quote, req SubmitRequest) (Order, error) { /* ... */ }

type OrderNotifier struct {
    bus  Bus
    smtp EmailClient
}
func (n OrderNotifier) Announce(ctx context.Context, order Order, to string) { /* ... */ }

func (p *OrderProcessor) Submit(ctx context.Context, req SubmitRequest) (Order, error) {
    if err := p.validator.Check(req); err != nil { return Order{}, err }
    quote, err := p.pricer.Quote(ctx, req)
    if err != nil { return Order{}, err }
    order, err := p.orders.Insert(ctx, quote, req)
    if err != nil { return Order{}, err }
    p.notifier.Announce(ctx, order, req.CustomerEmail)
    return order, nil
}
```

Counter-example: a long `applyDiscounts` whose body is a switch over coupon
kinds, each branch contributing to the same outcome (return the discounted
subtotal), has only one nameable job. Leave it as one function; splitting on
length alone would just relocate the switch.

---

## Consolidate to One Atomic Unit

### Detection

A single domain outcome that the business expects to commit-or-fail as one
is implemented as several sequential writes, each in its own transaction
(or each behind its own commit boundary in a non-DB substrate). Telltale
signs: two or more `BeginTx` / `Commit` pairs in the same method against
the same database handle with no external collaborator between them; a
"step 1 / step 2 / step 3" comment structure where each step is
self-contained but the caller treats the whole sequence as one outcome;
postmortems or runbooks describe partial-state recovery — "if the cancel
landed but the refund row is missing, run X" — for failures between the
steps.

### Smell

The fragmentation is structural, not domain-driven. The outcome the
business names is one thing ("cancel a membership", "place an order",
"transfer balance") but the implementation lets reality observe two or
three intermediate states the domain rules forbid. Every transient
failure between steps has to be reconciled by hand, and every new step
added to the flow widens the partial-state surface. The boundaries exist
because someone wrote the writes one at a time, not because the systems
involved cannot share a transaction.

### Refactor move

Collapse the sequential writes into a single transactional boundary: one
`BeginTx`, all the writes, one `Commit` with rollback on any error.
Helpers that previously owned a self-contained transaction become plain
functions taking the shared `Tx`. The atomic outcome the business
expects is now what the code enforces, and the partial-state recovery
work disappears. If part of the flow genuinely cannot share a
transaction (a remote API call, a write to a different store, a
non-transactional broker), keep that part outside and apply a saga /
outbox pattern to it — do not invent a distributed transaction.

### Example

Before:

```go
func (s *Service) Cancel(ctx context.Context, id MembershipID, reason string) error {
    tx1, _ := s.db.BeginTx(ctx)
    tx1.ExecContext(ctx, "UPDATE memberships SET status='canceled' WHERE id=$1", id)
    tx1.Commit()

    tx2, _ := s.db.BeginTx(ctx)
    tx2.ExecContext(ctx, "INSERT INTO refunds (membership_id, amount_cents) VALUES ($1,$2)", id, refund)
    tx2.Commit()

    tx3, _ := s.db.BeginTx(ctx)
    tx3.ExecContext(ctx, "INSERT INTO audit_log (subject_id, action, reason) VALUES ($1,$2,$3)", id, "membership.canceled", reason)
    return tx3.Commit()
}
```

After:

```go
func (s *Service) Cancel(ctx context.Context, id MembershipID, reason string) error {
    tx, err := s.db.BeginTx(ctx)
    if err != nil { return err }
    defer func() { _ = tx.Rollback() }() // no-op after Commit

    if _, err := tx.ExecContext(ctx, "UPDATE memberships SET status='canceled' WHERE id=$1", id); err != nil { return err }
    if _, err := tx.ExecContext(ctx, "INSERT INTO refunds (membership_id, amount_cents) VALUES ($1,$2)", id, refund); err != nil { return err }
    if _, err := tx.ExecContext(ctx, "INSERT INTO audit_log (subject_id, action, reason) VALUES ($1,$2,$3)", id, "membership.canceled", reason); err != nil { return err }
    return tx.Commit()
}
```

Counter-example: a `NotifyCanceled` that writes to a local outbox table
and then calls a remote billing API has two writes that cannot share a
transaction — the billing API is an external system the database cannot
enrol in `BeginTx` / `Commit`. Apply the outbox / saga pattern with
retries and idempotency keys; do not consolidate.
