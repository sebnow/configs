# Lenses: reasoning and sources

The reasoning behind each lens, with durable sources. The lens summaries in
SKILL.md are enough to apply them; read this when a lens is contested or you need
to defend a boundary choice.

## Direction of dependency

A dependency is knowledge: the depending module must know the interface it calls.
Point that knowledge deliberately. When the module that owns an outcome also
holds the decisions about it (when to retry, when to fall back, what to do next),
those decisions live in one place and the modules it leans on stay simple and
reusable. When a leaned-on module instead drives the caller's control flow, the
decision is smeared across the boundary and neither side can be understood alone.
Prefer depending on the parts that change least — a dependency on something
volatile drags its churn into you.

## Hide what varies

A boundary earns its place by hiding a decision that is likely to change, so that
reworking the code behind it does not ripple out to callers. Parnas argued a
module should hide the design decisions most likely to change; the interface is
the promise callers can rely on while the implementation moves.
(Parnas, *On the Criteria To Be Used in Decomposing Systems into Modules*, 1972.)
Inserting an interface specifically to let one implementation replace another
while the system keeps running is a standard, deliberate move.
(Fowler, *Branch By Abstraction*,
<https://martinfowler.com/bliki/BranchByAbstraction.html>.)
The counter-force is speculation: a boundary drawn around a decision that will
not vary is indirection with no payoff. A second real implementation justifies
the interface; the possibility of one does not.

## Cross the boundary in the consumer's terms

This is the "hide what varies" lens applied to the data that crosses, not just
the operations. The producer's internal shape — an upstream object, a transport
format, an unresolved id — is exactly the kind of detail that varies, so letting
it cross the boundary defeats the boundary. Keep resolution, transformation, and
formatting on the producing side, and pass only what the consumer needs, named in
the consumer's own terms. Then a change to the producer's internals, or to an
external format, stops at the boundary instead of reaching every consumer.

## Build the primitive first

Wrapping is one-directional. A direct, lower-level capability can be wrapped by a
richer one that holds it and offers more; the richer one cannot be unwrapped back
into the pieces it captured, because the state those pieces exposed now lives
inside the wrapper. So a design that ships only the convenience layer has
destroyed the option of the lower-level calls. Build the direct capability first
and let convenience compose over it.
(Muratori's criterion: never supply a higher-level operation that a few
lower-level ones could not trivially replace. Cheney, *Don't force allocations on
the callers of your API*,
<https://dave.cheney.net/2019/09/05/dont-force-allocations-on-the-callers-of-your-api>.)

## Expect leakage

Every non-trivial abstraction is leaky: the interface hides the shape, but timing,
failure behaviour, and edge-case semantics seep through even a well-drawn one.
(Spolsky, *The Law of Leaky Abstractions*,
<https://www.joelonsoftware.com/2002/11/11/the-law-of-leaky-abstractions/>.)
The freedom a boundary buys is real but bounded to the interface, not the full
observable behaviour. Naming what leaks keeps the design honest about which
reworks are actually safe.
