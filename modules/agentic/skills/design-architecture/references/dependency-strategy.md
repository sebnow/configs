# Judging whether a dependency warrants its own interface

A dependency varies by how much it can change out from under you. The more it
can, the more a boundary that hides it earns its place. Use the category to judge
whether to put an interface between the caller and the dependency — not to hide
what does not move.

## Pure computation or in-memory state

No external thing to vary. Do not put an interface between the caller and it; the
code is its own boundary. An interface here is indirection without payoff.

## A dependency with a ready stand-in

A dependency that has a lightweight local equivalent (an in-memory store, an
embedded database, a local file). Only introduce an interface if the stand-in and
the real thing diverge in shape. If the stand-in *is* the real type, use it
directly.

## Something you own across a process boundary

Your own service, database, or queue reached over a network. The wire shape and
availability vary independently of the caller, so a boundary that speaks the
caller's terms and hides the wire is usually worth it. What crosses is the
caller's data, not the transport.

## Something you do not control

A third party you cannot change. Its shape, failure modes, and availability can
all change without notice, so hiding it behind an interface in the caller's terms
is the strongest case for a boundary. Keep every provider-specific detail on the
far side.

## The one rule across all of them

A second real implementation justifies an interface; the possibility of one does
not. One implementation with no concrete second in sight is indirection with no
leverage — inline it and add the boundary when the second implementation actually
arrives.
