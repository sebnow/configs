# Visualize the architecture (optional)

Produce this only when the user asks to see or visualize the architecture. It is
part of the design conversation and comes before approval: a way to compare the
candidates and decide together, not a post-approval deliverable. It is never
required — the sketch stands on its own.

## Artifact

A single self-contained HTML file, written to the OS temp directory — never into
the repository or working tree, so nothing lands in version control. Create the
path with `mktemp` so it is unique, for example:

```sh
out="$(mktemp -t architecture-viz.XXXXXX).html"
```

or fall back to `${TMPDIR:-/tmp}/architecture-viz.html`. Write the file there and
report the path so the user can open it in a browser.

Everything is inline; the only external requests are the two CDNs below. Start
from [../assets/visualization-template.html](../assets/visualization-template.html)
(a read-only scaffold in the repo) and replace the example content with the real
modules, boundaries, and dependencies. Keep the token definitions, theme
handling, and legend.

## Stack

- **Tailwind (CDN)** for page layout and utility styling.
- **Mermaid (CDN)** for graph-shaped diagrams.
- **Hand-built CSS/SVG** for editorial visuals.
- **Modern CSS over JavaScript.** Use CSS custom properties, grid, container
  queries, `:has()`, `<details>`, and CSS transitions/animations. Reserve
  JavaScript for what genuinely needs it: rendering Mermaid and re-theming it on
  a mode switch. No JS for layout, collapse, or before/after toggling — CSS does
  those.

## Mermaid vs hand-built

Pick per visual, and mix freely within one page.

Use **Mermaid** when the relationship is graph-shaped:

- dependency graphs — which module depends on which, and which way
- call flows — control moving through the change
- sequences — ordered interactions over time

Hand-build with **divs/SVG** when the point is editorial, not a graph:

- mass / depth diagrams — how much complexity sits behind an interface
- cross-sections — what a boundary hides beneath it
- collapse / merge animations — one module absorbing another

## Per candidate

When the design explored alternatives, each candidate is self-contained: render
it as its own `<article>` (a stacked card, or a pure-CSS tab via radio inputs and
`:has()` — no JS). An article carries its whole context so it can be read alone:

- **Files / modules involved** — the units the candidate touches, as chips.
- **Problem** — what this candidate is solving.
- **Solution** — what the candidate is, in one or two sentences.
- **Benefits** — what it buys (and, where useful, what it costs).
- **Before / after diagram** — see below.

Order the articles by recommendation, strongest first, matching the sketch.

## Before/after

Every candidate gets a before and an after, shown together inside its article.
Before is the current or naive shape; after is the designed shape. For a
greenfield change with no prior code, before is the status-quo or naive
arrangement the design improves on. Use the same visual language on both sides so
the delta reads at a glance.

## Visual language

Drive every visual from the shared design tokens in the template so one concept
always looks the same, and render a legend from those tokens so the reader can
decode the page. Keep this mapping:

| Concept                    | Visual treatment                          | Token         |
| -------------------------- | ----------------------------------------- | ------------- |
| Module (owned)             | solid filled card, solid border           | `--module`    |
| Boundary                   | labeled enclosure, accent border          | `--boundary`  |
| External system (not owned)| hatched fill, distinct hue                | `--system`    |
| Existing element           | muted / desaturated                       | `--existing`  |
| New element                | accent fill, `+` badge                    | `--new`       |
| Removed element            | dashed border, struck label               | `--removed`   |
| Dependency                 | arrow; direction is the arrowhead         | `--edge`      |
| What crosses the boundary  | label chip on the edge                    | `--crosses`   |

Map Mermaid to the same tokens through `themeVariables` so its diagrams match the
hand-built ones. Show dependency direction and what crosses on every edge.

## Modes

Support light and dark. Initialise from `prefers-color-scheme`, offer a toggle,
and re-theme Mermaid to match. Every colour comes from the theme tokens — no
hard-coded hex outside the `:root` token blocks.
