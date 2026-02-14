# Testing Framework

Test skills across three areas before finalizing.

## Triggering Tests

Verify the skill activates when it should and stays silent when it should not.

Test cases:

- **Obvious tasks** -
  Direct requests matching the skill's description.
  Target: skill activates in 90%+ of attempts.
- **Paraphrased requests** -
  Same intent expressed differently.
  Target: skill activates without exact keyword matches.
- **Negative triggers** -
  Unrelated topics the skill should not activate for.
  Target: zero false activations.

Example test suite:

```
Trigger: "create a new skill for code review"    → should activate
Trigger: "write a SKILL.md for deployment"        → should activate
Trigger: "help me write a README"                 → should NOT activate
Trigger: "review this pull request"               → should NOT activate
```

## Functional Tests

Verify the skill produces correct outputs
and handles edge cases.

Test cases:

- **Valid outputs** -
  Skill produces correctly structured results
  matching the specification.
- **Tool calls succeed** -
  Any tool calls the skill triggers complete without errors.
  Target: zero failed API or tool calls.
- **Edge cases** -
  Unusual inputs, empty inputs, or boundary conditions
  produce reasonable results or clear error messages.

## Performance Comparison

Compare agent behavior with and without the skill.

Metrics to track:

- **Quantitative** -
  Tool call count, error rate, completion rate,
  number of user corrections needed.
- **Qualitative** -
  No additional user prompting needed for the happy path,
  consistent results across multiple runs,
  agent follows the intended workflow order.

Comparison format:

```
Without skill: [observed behavior, failure modes]
With skill:    [improved behavior, metrics]
Delta:         [specific improvements]
```
