---
id: 01-type-conversion-struct
title: Layout-compatible structs use Go type conversion
fixture: fixtures/struct-layout-conversion
expect_trigger: true
target_lens: null
category: pressure
rationale: |
  WorkflowInput/ActivityInput and ActivityOutput/WorkflowOutput are each
  declared as separate types at different layers but share an identical
  field layout. The Go idiom for crossing such a layer boundary is the
  type-conversion expression `T(other)`. Without the idiom encoded in the
  coding skill, agents tend to write field-by-field assignments
  ("more explicit"), which costs five lines per direction and silently
  rots when fields are added on only one side. With the idiom, the
  implementation collapses to two casts: `ActivityInput(in)` going in
  and `WorkflowOutput(out)` coming back.
assertions:
  - id: type-conversion-used
    text: |
      The agent's implementation of `Run` constructs the ActivityInput
      using a Go type-conversion expression `ActivityInput(in)` (or
      `ActivityInput(input)`), not by listing each field individually
      such as `ActivityInput{TenantID: in.TenantID, ClientID: in.ClientID, ...}`.
  - id: output-conversion-used
    text: |
      The agent's implementation returns the workflow output using a
      Go type-conversion expression `WorkflowOutput(out)` (or whatever
      the activity-output variable is named), not by listing each
      WorkflowOutput field individually.
  - id: signature-preserved
    text: |
      The agent does not change the signature of `ProcessActivity` or
      collapse `WorkflowInput`/`ActivityInput` (or the matching output
      pair) into a single shared type. The two types remain declared
      separately; only the conversion at the boundary is cheap.
---

Implement the body of `Run` in `workflow.go` so it invokes `ProcessActivity`
with the workflow input and returns the activity result as a `WorkflowOutput`.
Propagate any error from `ProcessActivity` unchanged.
