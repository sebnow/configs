This is not the `council` skill.
`design-alternatives` generates alternative interfaces for one chosen boundary.
`council` runs multi-perspective deliberation on a broader decision (scope, risk, strategy).

## Process

### Phase 1: Frame the problem space

Before spawning sub-agents, write down:
- The module's callers: who uses it, how often, what they need.
- The complexity it must absorb: what callers should not need to know.
- The constraints: performance, error handling, extension points.

This frame is passed verbatim to each sub-agent as shared context.

### Phase 2: Spawn sub-agents

Spawn three or more sub-agents in parallel.
Each receives the frame from Phase 1 plus one design constraint.

Required sub-agents:
- **Minimise interface** — fewest parameters, fewest types, fewest error modes.
- **Maximise flexibility** — most extension points, most composable pieces.
- **Optimise for the common caller** — best ergonomics for the 80% case.
- **Isolate the volatile dependency** (when an external dependency is involved) —
  draw the interface so the dependency's changes stop at the boundary.

Each sub-agent produces:
- the interface: types, signatures, invariants, error modes
- a usage example showing the common caller path
- what the implementation hides
- how the interface handles its dependencies (see dependency-strategy.md)
- trade-offs: what this design sacrifices

### Phase 3: Present and compare

Compare designs by:
- **Depth**: leverage at the interface — how much does one call absorb?
- **Locality**: where does change concentrate when requirements shift?
- **Boundary placement**: where is the API boundary drawn, and what does each side own?

Do the analytical work and put it to the user — do not resolve it for them.
State what each design trades, where they are in tension, and which calls turn on
context you do not have. Recommend a view where you hold one and argue it; a flat,
neutral menu is a cop-out, but so is a defended conclusion that ends the discussion.
LLMs are unreliable at design — surface what you are unsure of rather than papering
over it with confidence. The alternatives stay live until the user chooses; the
boundary is decided when they decide it, not when you recommend.
