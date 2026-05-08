A small Go module that loads moderation rulesets from Postgres and applies them
to incoming events.

The `Ruleset` struct's rule fields (`RuleID`, `RuleSequence`, `RuleIsTerminal`)
are declared as `*string`/`*uint32`/`*bool` because the historical loader used
`LEFT JOIN rule`. Two database migrations ago an `INNER JOIN`-equivalent
constraint was added: every row returned by `LoadActiveRulesets` is guaranteed
to have a rule. Every caller of `Apply` reaches it through `LoadActiveRulesets`,
so the rule pointers are never nil at this point in the program. The pointer
types are a leftover from the pre-migration loader.

Implement `Apply` to return whether the rule terminated processing.
