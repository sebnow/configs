---
name: council
description: "Multi-perspective deliberation through independent parallel analysis. Spawns subagents with different analytical lenses to stress-test decisions. Use when evaluating complex decisions, comparing approaches, or pressure-testing strategies. Triggers: 'council', 'deliberate', 'stress-test this decision', 'get perspectives on', 'pressure-test'. Do NOT use for brainstorming (use brainstorm), code review (use review-code), or straightforward decisions with clear answers."
---

# Council

Independent multi-lens analysis with cross-model synthesis.
Stress-tests a decision by analyzing it
through multiple analytical lenses in parallel,
then synthesizing the contradictions.

Not brainstorming — brainstorming explores what to do.
Council pressure-tests a decision you are already considering.

Not code review — code review evaluates correctness of code.
Council evaluates the reasoning behind a decision.

## Instructions

### Step: Understand the Decision

Extract the decision or question from the user's request.
If the question is too vague to analyze concretely,
ask the user to narrow it before proceeding.

A good council question names a specific choice
with identifiable trade-offs.
"How should we design our system?" is too vague.
"Should we use a message queue or direct HTTP calls
between these two services?" is concrete.

### Step: Select Lenses

Choose 2-3 analytical lenses based on the question's domain.
State the selected lenses with a brief rationale.

Each lens is a concrete analytical instruction —
what to look for, what to surface, what to ignore.
Lenses should be methodologically distant from each other.
Two lenses that both do trade-off analysis
with different labels are not genuinely different.

Predefined lens sets by domain:

**Architecture decisions:**
- Failure modes: identify how this fails under load,
  under partial outage, under operator error.
  What breaks first? What is the blast radius?
- Scaling assumptions: what must be true about usage patterns
  for this to work at 10x? What implicit assumptions
  about data volume, concurrency, or growth rate are baked in?
- Operational cost: what does this cost to run, monitor,
  debug, and migrate away from?
  What expertise does the team need that it may not have?

**Strategy decisions:**
- Opportunity cost: what are you not doing by choosing this?
  What options does this foreclose?
  What would you do with the time and resources instead?
- Second-order consequences: what happens after this succeeds?
  What happens after it fails?
  What feedback loops does this create or break?
- Inversion: what would guarantee this fails?
  Work backward from failure to identify hidden risks.

**Risk assessment:**
- Tail exposure: what is the worst realistic outcome,
  not the expected outcome?
  Is the downside bounded or unbounded?
- Assumption fragility: which assumptions are load-bearing?
  What happens when each one turns out to be wrong?
- Recovery paths: if this is the wrong choice,
  how do you reverse it? What is the cost of being wrong?

**General** (use when no domain fits):
- Hidden assumptions: what is everyone taking for granted?
  What beliefs are load-bearing but unexamined?
- Systemic effects: what are the second-order consequences?
  What feedback loops exist?
  How does this interact with the rest of the system?
- Practical constraints: what does this require in practice?
  What operational, organizational, or resource constraints
  does the analysis ignore?

The user may also describe lenses in their own words.
Interpret the request and construct appropriate lenses.
If the user does not specify, select the best-fit domain
or construct lenses from the question's specifics.

### Step: Analyze in Parallel

Spawn one subagent per lens.
Each subagent receives:

- The decision question with full context
- Its specific lens instruction
- The output format below

Subagents run in parallel.
They never see each other's output.

For each lens,
spawn an agent via the shell
with a prompt containing the lens instruction
and the decision context.

If spawning subagents fails or is unavailable,
fall back to analyzing each lens sequentially.
Label each analysis clearly with its lens name.

Each subagent outputs in this format:

```
## [Lens Name]

### Stance
[For / Against / Conditional] on the decision, from this lens.

### Analysis
[2-4 paragraphs applying the lens to the decision.
Focus only on what this lens reveals.
Do not attempt balanced analysis —
that is the coordinator's job.]

### Key Risk If Ignored
[The single most important thing the decision-maker
would miss without this lens. One paragraph.]

### Confidence
[High / Medium / Low] — [one sentence justification]
```

### Step: Synthesize

Collect all analyses.
Surface contradictions between them.

Do not pick a winner.
The value is in the tension, not the resolution.

Structure the synthesis as:

```
## Council Verdict

### Analyses
[Per-lens summary — 2-3 sentences each,
preserving each lens's stance and core argument]

### Contradictions
[Where lenses disagree and why.
Name the specific tension, not just "they disagree."]

### Unresolved
[Judgment calls that remain genuinely contested.
Questions the analysis surfaced but could not answer.]

### Decision
[Recommendation with conditions,
or explicit "no consensus" with the tension preserved.
If recommending, state what would change the recommendation.]
```

Where lenses disagree on verifiable facts
(not judgment — facts),
resolve with tool calls before synthesizing.
Run the test, grep the code, check the documentation.
Include the evidence in the verdict under a **Resolved** section
between Contradictions and Unresolved.

## Common Issues

### Homogeneous analysis

Cause: lenses are too similar
(two lenses both doing trade-off analysis with different labels).
Solution: increase analytical distance.
A failure-mode lens and an opportunity-cost lens
ask fundamentally different questions.
A "pros" lens and a "cons" lens do not.

### Vague analysis

Cause: the decision question is not specific enough.
Solution: ask the user to narrow before proceeding.
"Should we use microservices?" produces vague analysis.
"Should we extract the billing service
given our current team size and deployment constraints?"
produces concrete analysis.

### Premature convergence

Cause: all lenses reach the same conclusion.
This may be correct, but treat it as suspicious.
Solution: note the agreement in the verdict
and flag it as "convergence — consider whether
a lens was missed or the question is already decided."
