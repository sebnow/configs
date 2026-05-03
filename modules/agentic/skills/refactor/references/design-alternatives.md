This is not the `council` skill.
`design-alternatives` generates alternative interfaces for one chosen refactoring candidate.
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
- **Ports-and-adapters** (if external dependencies are involved) —
  define the port and identify the adapters.

Each sub-agent produces:
- the interface: types, signatures, invariants, error modes
- a usage example showing the common caller path
- what the implementation hides
- the dependency strategy and adapters (see dependency-strategy.md)
- trade-offs: what this design sacrifices

### Phase 3: Present and compare

Compare designs by:
- **Depth**: leverage at the interface — how much does one call absorb?
- **Locality**: where does change concentrate when requirements shift?
- **Boundary placement**: where is the API boundary drawn, and what does each side own?

Give a recommendation with reasoning.
Do not present a neutral menu — commit to a preferred design and defend it.
The recommendation becomes the agreed design for Phase 3 of the refactor process.
Hand off to the coding skill for implementation.
