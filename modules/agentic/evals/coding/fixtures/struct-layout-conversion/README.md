Small Go package wiring a workflow-shaped function to an activity.

The fixture defines two pairs of types — `WorkflowInput`/`ActivityInput` and
`ActivityOutput`/`WorkflowOutput`. Each pair shares an identical layout:
field names, types, and order line up exactly. The two types in each pair
are kept separate because they live at different layers. The
`Run(WorkflowInput) (WorkflowOutput, error)` stub needs to call
`ProcessActivity` and return its result, mapping between the two pairs.
