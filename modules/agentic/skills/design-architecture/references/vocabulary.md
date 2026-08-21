# Vocabulary

Plain terms for reasoning about boundaries.

## module

A unit of code with an interface and a hidden implementation.
The boundary is the interface, not the file — a module can be a function, type,
file, package, process, or service.

## interface

What a caller must know to use a module: the operations, the shapes that cross,
the invariants, the failure modes. Everything else is implementation.

## implementation

What sits behind the interface. A caller is insulated from implementation
changes as long as the interface holds.

## boundary

Where two modules meet: one depends on the other through an interface.
A boundary is worth drawing only when it hides a decision that varies.

## dependency direction

Which module knows about which. The direction is a design choice, not an
accident — point it deliberately (see the Direction lens).

## depth

How much complexity a module absorbs per unit of interface it exposes.
A deep module hides a lot behind a small interface; a shallow one exposes
nearly as much as it hides. Depth is leverage at the interface, not lines of
code.

## leverage

How much one call does on the caller's behalf — how much the caller need not
understand.

## locality

Whether a change in requirements touches one place or many. High locality: a
new variant is one edit. Low locality: edits scatter.
