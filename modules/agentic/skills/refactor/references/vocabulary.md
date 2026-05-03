## module

A unit of code with a defined interface and hidden implementation.
The boundary is the interface, not the file system —
a module can be a function, class, file, package, or service.
Avoid: "component" (overloaded in UI frameworks), "class" (implementation detail).

## interface

The surface through which a caller interacts with a module:
types, function signatures, invariants, and error modes.
The interface is what a caller must know; the implementation is what they must not.
Avoid: calling a module's surface its "API" — that implies a published, cross-process contract.
Use "interface" for the exposed surface; use "API boundary" for a named boundary between modules.

## implementation

Everything behind the interface.
Callers are insulated from implementation changes
as long as the interface is stable.

## API boundary

A named boundary where two systems or modules meet under a defined contract.
Both sides can evolve independently across an API boundary.
Avoid: "seam" — see Rejected framings.

## depth

The ratio of complexity absorbed to interface surface exposed.
A deep module absorbs significant complexity behind a simple interface.
A shallow module exposes nearly as much complexity as it hides.
Avoid: measuring depth as a lines-of-code ratio —
a 500-line module with a 3-parameter interface is deeper than
a 50-line module with a 10-parameter interface.
Depth is leverage-at-interface, not lines.

## leverage

How much complexity one call absorbs on the caller's behalf.
High leverage: one call does substantial work the caller need not understand.
Avoid: "power" or "utility" (subjective; not measurable at the interface).

## locality

The property that a change in requirements touches few modules.
High locality: a new variant requires editing one place.
Low locality: a new variant requires edits scattered across many modules (shotgun surgery).

## adapter

A module that translates between two interfaces without adding domain logic.
An adapter wraps an external dependency and implements a port.
Avoid: "wrapper" — adapters may transform; wrappers imply shallow pass-through.

## port

An interface defined at an API boundary that allows swapping implementations.
A port is justified only when two or more adapters exist or are concretely planned.
Avoid: "interface" when you mean port specifically —
all ports are interfaces, but not all interfaces are ports.

## Rejected framings

### "seam"

Do not use "seam" as the general term for a swap point or boundary.
Feathers defines a seam as a place to swap behavior without changing the caller.
Matt Pocock's usage in TypeScript refers to a different concept.
Hexagonal architecture formalises the Feathers-style seam as a port+adapter pair.
Using "seam" for all three conflates distinct ideas.
Use "API boundary" for the named boundary,
"port" for the swappable interface,
and "adapter" for the swapping implementation.

### Depth as lines-of-code ratio

Depth is not lines-behind-interface divided by lines-in-interface.
A module is deep if it absorbs significant complexity per unit of interface surface.
Measure: interface surface (number and complexity of parameters, types, error modes)
versus complexity absorbed (logic, state, error handling, coordination hidden from callers).
