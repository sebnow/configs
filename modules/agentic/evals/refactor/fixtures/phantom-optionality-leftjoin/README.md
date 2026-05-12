Postgres-backed repository that loads rulesets and their rules.

The query is a `LEFT JOIN ruleset → rule`, so a ruleset row may come back with
all rule columns NULL. The Go scan reflects that: every rule field is a `*T`,
followed by a 25-line block that copies `*ruleID` etc. into a `Rule` struct
only when `ruleID != nil`. In practice the codebase never displays a ruleset
with zero rules — the domain forbids it. The pointers and the nil-check block
exist only because the JOIN, not the domain, permits the empty case.
