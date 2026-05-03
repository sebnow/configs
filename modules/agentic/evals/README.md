# Agentic Skill Evals

Manual eval harness for agentic skills.
Run it on demand after any change to a skill, a referenced file, or the underlying model.
Each scenario invokes the skill via `pi`, then a second `pi` call judges the transcript
against per-scenario assertions.

## First-time setup

Make the scripts executable (only needed once after checkout):

```bash
chmod +x modules/agentic/evals/run.sh modules/agentic/evals/lib/*.sh
```

## Prerequisites

- `pi` on PATH (pi-coding-agent)
- `jq`
- `md` (markdown query CLI)
- `python3`
- `ANTHROPIC_API_KEY` set

## Run all refactor scenarios

```bash
modules/agentic/evals/run.sh \
  --skill modules/agentic/skills/refactor \
  --eval-dir modules/agentic/evals/refactor
```

## Run a single scenario

```bash
modules/agentic/evals/run.sh \
  --skill modules/agentic/skills/refactor \
  --eval-dir modules/agentic/evals/refactor \
  --filter '01-shallow-passthrough'
```

Glob patterns work: `--filter '0*'` runs all single-digit-prefixed scenarios.

## Run the smoke test

Verifies the harness itself works end-to-end before running real scenarios:

```bash
modules/agentic/evals/run.sh \
  --skill modules/agentic/evals/_smoke/ping-smoke \
  --eval-dir modules/agentic/evals/_smoke
```

Both assertions should PASS and the runner should exit 0.

## Add a scenario

1. Copy an existing scenario file from `evals/<skill>/scenarios/`.
2. Edit the frontmatter: give it a unique `id`, set `fixture`, `category`,
   and write `assertions` that are falsifiable from a transcript.
3. Write the prompt body after the closing `---`.
4. If the scenario needs source files, create a fixture directory under
   `evals/<skill>/fixtures/<name>/` with a `README.md` giving the agent
   one paragraph of context.
5. Iterate with `--filter '<your-id>'` until the assertions behave as expected.

## Cost expectations

These are first-run estimates; replace with measured numbers after the first full run.

- Subject `pi` run: ~30–150k tokens per scenario
- Judge `pi` run: ~5–15k tokens per scenario
- Full 11-scenario refactor suite: roughly a few dollars and 30–45 minutes wall time

## Reading results

- Stdout shows a pass/fail table with per-assertion failure reasons.
- Per-scenario artifacts land in `.eval-runs/<utc-ts>/<scenario-id>/`:
  - `subject.stdout` — pi stdout (useful for debugging hangs or crashes)
  - `subject.txt` — flattened transcript used as judge input
  - `judge-prompt.txt` — full prompt sent to the judge
  - `verdict.json` — structured verdict with per-assertion statuses
  - `judge-raw.txt` — present only when the judge produced no valid JSON block
- Pass `--keep-transcripts` to also retain `subject.jsonl`.

## Non-determinism caveat

Judge verdicts are non-deterministic.
Before treating a FAIL as a regression,
re-run with `--filter '<id>'` once or twice.
If it flips, tighten the assertion text rather than acting on a single run.
