---
id: 06-push-invariants-data-shape
title: Nullable-but-always-set fields tightened in the data shape
fixture: fixtures/nullable-pointer-always-set
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  `Ruleset.RuleID`, `Ruleset.RuleSequence`, and `Ruleset.RuleIsTerminal`
  are declared as `*string`/`*uint32`/`*bool`, but the README and the
  `Apply` doc comment both state that every value reaching `Apply` is
  guaranteed (by a Postgres constraint) to have a rule attached. The
  defensive option is to add `if rs.RuleIsTerminal != nil` guards inside
  `Apply`. The data-shape option is to drop the pointers from the
  struct: `string`/`uint32`/`bool` non-pointer fields make it impossible
  to construct a `Ruleset` without a rule, and the question "what if
  the field is nil" stops being askable. Without the
  push-invariants-into-the-data-shape rule encoded in the coding skill,
  agents tend to dereference the pointers behind nil guards and leave
  the struct shape unchanged — the implementation works but it locks
  in a state the domain forbids.
assertions:
  - id: tightens-struct-fields
    text: |
      The agent proposes changing at least `Ruleset.RuleIsTerminal` (and
      ideally also `Ruleset.RuleID` and `Ruleset.RuleSequence`) from a
      pointer type (`*bool`/`*string`/`*uint32`) to its non-pointer form
      (`bool`/`string`/`uint32`). The change must land in the struct
      declaration itself — adding a separate non-nullable wrapper type
      or a parallel struct does not satisfy this assertion. The agent
      must edit `ruleset.go` and remove the pointer indirection on the
      struct field, not merely propose it as a future option.
  - id: no-defensive-nil-check
    text: |
      The agent's final implementation of `Apply` does not contain a
      `nil` guard against `rs.RuleIsTerminal`, `rs.RuleID`, or
      `rs.RuleSequence` (e.g. `if rs.RuleIsTerminal == nil { ... }`,
      `if rs.RuleIsTerminal != nil { ... }`). The implementation
      returns the terminal flag without first inspecting whether the
      pointer is nil, because after the data-shape change the pointer
      no longer exists.
  - id: cites-data-shape-rule
    text: |
      The agent explicitly justifies the change by pointing at the
      data shape — phrases like "push the invariant into the data
      shape", "make the data such that the code can't be wrong",
      "change the data model so the nil check is impossible",
      "tighten the type", or "the domain forbids this state, so the
      type should too" all satisfy this assertion. Justifying the
      change purely as "simpler" or "cleaner" without naming the
      data-shape principle does not satisfy this assertion.
---

Implement `Apply` in `ruleset.go` so it returns whether the ruleset's rule
terminated processing for this event. Treat the `Event` argument as
metadata for now — you do not need to evaluate the rule against the event
content; just return the terminal flag from the ruleset's rule. Make any
adjustments to the surrounding code that the implementation requires.
