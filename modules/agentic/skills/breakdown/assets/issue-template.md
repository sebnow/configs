# Issue Template

Each issue uses this structure in order.
Do not reorder sections. Do not omit sections.

---

## [Short title: behavior delivered, not layer touched]

### Context

Background the implementing agent needs:

- Why this slice exists and which feature it belongs to
- Relevant API boundaries — existing (types, signatures, contracts) or proposed advisory ones.
  If the PRD supplied a signature for this module, copy it verbatim:

  ```ts
  type CreateTask = (input: { title: string; priority: 1 | 2 | 3 | 4 }) => Promise<Task>
  ```

  Inline references to a single name, route, or symbol stay in backticks (e.g. `` `GET /health` ``).
- File paths as orientation hints, labeled: "orientation hint: `path/to/file`"
- Links to source documents (PRD, brainstorm transcript) as inline markdown links

### Details

What to implement: the specific end-to-end behavior this slice delivers.
Concrete enough that the agent does not need to re-derive scope.
Include the affected layers and how they connect.

### Acceptance Criteria

Exact conditions that prove this slice is done.
Each criterion is a specific observable behavior, output, or metric.
Not general engineering standards.

Example format:
- `GET /health` returns `200 { "status": "ok" }`
- The export button is disabled when zero rows are visible
- Navigating to `/profile` without a session redirects to `/login`

### Blocked by

List only if this issue has dependencies.
For each dependency:

- **[Issue title]** — why this dependency exists; under what circumstances this issue could proceed without it

Omit this section entirely if there are no dependencies.
