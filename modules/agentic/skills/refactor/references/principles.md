## Shallow modules

Detection: the function signature has nearly as many parameters or type dependencies
as its body; callers must understand most of what the module does in order to use it.

Smell: the module does not absorb complexity — it re-exposes it.
Callers gain nothing from the indirection.

Refactor move: merge the module into its callers,
or deepen it by pulling the complexity callers currently manage
into the module's body.

### Example

Before:
```go
// caller must manage the validation flag
func SaveUser(db *sql.DB, u User, validate bool) error {
    return persist(db, u, validate)
}
```

After:
```go
// validation is always on; caller no longer manages it
func SaveUser(db *sql.DB, u User) error {
    if err := u.Validate(); err != nil {
        return err
    }
    return db.Exec("INSERT ...", u.Name, u.Email)
}
```

---

## Shotgun surgery

Detection: grep for the concept.
If results scatter across many files that all need touching for one logical change,
it is shotgun surgery.

Smell: no single place owns the concept.
A new variant means hunting and patching across the codebase.
Wrapping the exploded parameters in a config struct is not a fix —
adding a sixth field still requires edits everywhere.
The fix is to name the variants so a new one requires one edit.

Refactor move: introduce a discriminated union, state machine, or hoisted decision.
All variant-specific logic lives in one place; callers dispatch through it.

### Example

Before:
```ts
// isAdmin, isPremium, isModerator checks scattered across many files
if (user.role === "admin") { showAdminPanel() }
if (user.role === "premium") { showPremiumBadge() }
```

After:
```ts
// exhaustive switch in one place; new roles require one edit here
function applyRoleUI(role: Role): void {
    switch (role) {
        case "admin":    return showAdminPanel()
        case "premium":  return showPremiumBadge()
        case "moderator": return showModeratorTools()
    }
}
```

---

## Hoisting decisions

Detection: the same condition appears on every traversal of a tree,
inside a loop, or down a call chain.

Smell: the decision is evaluated redundantly.
The structural problem is that the decision does not belong at the traversal point.

Refactor move: evaluate the decision once at the assembly point —
where the tree is built, the loop starts, or the call chain is configured.
Pass the result down, not the condition.

### Example

Before:
```python
for item in items:
    if config.debug:   # evaluated N times
        log(item)
    process(item)
```

After:
```python
logger = debug_logger if config.debug else noop_logger  # evaluated once
for item in items:
    logger(item)
    process(item)
```

---

## Compression

Detection: two or more functions share identical or near-identical logic.
Git blame showing copy-paste history is a strong signal.

Smell: a bug in the shared logic must be fixed in multiple places.

Refactor move: extract the shared logic into a single module.
Callers call it. The module earns its keep immediately.

### Example

Before:
```go
// CreateUserHandler
if len(req.Name) == 0 || len(req.Name) > 100 { return errInvalidName }
if !emailRE.MatchString(req.Email) { return errInvalidEmail }
if req.Age < 18 || req.Age > 120 { return errInvalidAge }

// UpdateUserHandler — same invariants, copy-pasted
if len(req.Name) == 0 || len(req.Name) > 100 { return errInvalidName }
if !emailRE.MatchString(req.Email) { return errInvalidEmail }
if req.Age < 18 || req.Age > 120 { return errInvalidAge }
```

After:
```go
func (r UserRequest) Validate() error {
    if len(r.Name) == 0 || len(r.Name) > 100 { return errInvalidName }
    if !emailRE.MatchString(r.Email) { return errInvalidEmail }
    if r.Age < 18 || r.Age > 120 { return errInvalidAge }
    return nil
}
// a change to the age bounds now requires one edit, not N
```

---

## Premature abstraction

Detection: an interface with exactly one implementation;
a helper function called from exactly one site;
a generic type parameter always instantiated to the same type.

Smell: the abstraction adds indirection without hiding complexity.
Callers follow the indirection without gaining leverage.

Refactor move: collapse the interface into its one implementation.
Inline the helper. Remove the unused generic parameter.
Wait for a second concrete use case before re-introducing the abstraction.

### Example

Before:
```go
type Storer interface { Store(u User) error }
type DBStorer struct{}
func (d *DBStorer) Store(u User) error { ... }
// one caller, one implementation, no variation
```

After:
```go
type UserStore struct{}
func (s *UserStore) Store(u User) error { ... }
```

---

## Non-pessimization

Detection: a linear scan on a container that is always small and fixed;
virtual dispatch on a type that has exactly one variant at runtime;
a heap allocation inside a loop that runs many times.

Smell: the structure forces gratuitous overhead the language does not require.
This is not micro-optimisation — it is removing structural waste.

Refactor move: use the data structure or dispatch mechanism the structure warrants.
Only structural causes are in scope.
Do not measure runtime performance, benchmark, or micro-optimise.

### Example

Before:
```go
func HandleRequest(w http.ResponseWriter, r *http.Request) {
    client := &http.Client{Timeout: 5 * time.Second} // new client per request
    resp, err := client.Get(upstreamURL)
    ...
}
```

After:
```go
var upstream = &http.Client{Timeout: 5 * time.Second} // shared; connection pool reused

func HandleRequest(w http.ResponseWriter, r *http.Request) {
    resp, err := upstream.Get(upstreamURL)
    ...
}
```

---

## Learning ladder (gated)

**Gate: apply this lens only when consumers are not fully visible —
a published library, external package, or service with callers outside this repo.
If every caller is inside this repo and visible to grep,
skip this lens and apply compression and premature-abstraction instead.**

Detection: the high-level convenience function is the only path to the underlying capability.
Callers who need partial behaviour must use the convenience API and work around it.

Smell: the learning ladder is broken.
A user cannot graduate from convenience to lower-level pieces
because those pieces are not exposed.

Refactor move: expose the lower-level modules behind a stable API boundary.
The convenience function becomes a thin composition of them.

### Example

Before:
```ts
// internals not exposed; only path for all callers
export function fetchAndRenderUser(id: string): Promise<void> { ... }
```

After:
```ts
export async function fetchUser(id: string): Promise<User> { ... }
export function renderUser(user: User): void { ... }
// convenience wrapper remains; lower-level pieces now accessible
export async function fetchAndRenderUser(id: string): Promise<void> {
    renderUser(await fetchUser(id))
}
```
