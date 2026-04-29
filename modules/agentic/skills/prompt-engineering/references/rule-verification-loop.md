# Rule Verification Loop

A red-green-refactor loop for refining a written rule when an agent
following it produces output that diverges from the intended outcome.

Use this when you have ground truth (a desired output, an expected
classification, a correct verdict) and a rule that should have produced
it. The question is whether the rule, as written, is sufficient — not
whether the author of the rule knows the intent.

## Why a fresh subagent

Self-tests miss the failure mode this loop catches: author-judgement
creep. A rule "works" in conversation because the author already knows
what they meant; the surrounding context fills in the gaps. A fresh
subagent given only the rule text — no conversation history, no
surrounding skill body, no commentary — exposes whether the rule itself
carries the intent.

This is the difference between "I think this rule says X" and "an agent
reading only this rule produces X".

## Loop

For each observed divergence (one delta = one loop iteration):

1. Read the rule as currently written. Predict what an agent following
   only that rule would produce. If the prediction matches the
   divergent output, the rule is the problem — not just the agent.
2. Make the smallest edit to the rule that addresses this delta. Do
   not generalize beyond the observed case.
3. Spawn a verification subagent given ONLY the updated rule. Ask it
   to classify or apply the disputed case. If wrong, sharpen — usually
   by adding the misclassified case verbatim with the correct verdict
   (see Few-Shot Examples in SKILL.md).
4. Iterate until the verification subagent's output matches ground
   truth on the disputed case.

## End-to-end check

After per-delta refinements, run a final integration test: hand a fresh
subagent the original divergent input and only the updated rules. Ask
it to redo the task. Diff against ground truth.

Iterate until only micro-stylistic differences remain (anchor wording,
em-dash vs comma, ordering of equally-valid options) — not rule-level
gaps. Rule-level gaps mean per-delta refinement is incomplete; loop
back.

## When this applies

- Refining a writing-style guide from a draft/revision pair
- Tuning a skill's enforcement language after observing it failed to
  enforce
- Any case where you have a wrong output, a correct output, and a rule
  that should have produced the correct one

## Common loopholes

- Testing the rule in conversation with the author. The author's
  context fills in what the rule omits. Use a fresh subagent.
- Loading the surrounding skill body or related rules into the
  subagent. The other content may carry the signal you are trying to
  attribute to this rule.
- Generalizing the edit beyond the observed delta. Each loop iteration
  addresses one case. Generalization happens later, by accumulation,
  not in the same edit.
- Rewriting the abstract definition in response to a misclassification.
  Worked examples discriminate; longer definitions do not. See the
  Few-Shot Examples section in SKILL.md.
