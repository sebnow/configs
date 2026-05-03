## In-process

Applies to: pure computation, in-memory state, or logic with no external dependencies.

Test approach: test directly. No adapter needed. No port needed.
The module is its own test target.

---

## Local-substitutable

Applies to: dependencies with a lightweight local stand-in
(in-memory filesystem, embedded database, local file store).

Test approach: test the deepened module with the local stand-in.
No port needed if the stand-in is the real dependency type.
Introduce a port only when the stand-in and production dependency diverge structurally.

---

## Remote-but-owned

Applies to: your own services across a network boundary
(internal microservices, owned APIs, internal message queues).

Test approach:
1. Define a port (interface) at the API boundary.
2. Inject the real HTTP/gRPC adapter in production.
3. Inject an in-memory adapter in tests.

The in-memory adapter must conform exactly to the port contract.
Divergence between the in-memory and real adapter is a test-validity risk.

---

## True-external

Applies to: third-party services you do not control
(payment processors, email providers, external SaaS APIs).

Test approach:
1. Define a port at the API boundary.
2. Inject a mock adapter in tests.
3. Run at least one integration test against the real service in CI.

The mock adapter should enforce the port contract, not just return happy-path data.

---

## Seam discipline

One adapter is a hypothetical port.
Two adapters is a real one.

Do not introduce a port unless at least two adapters are justified now.
"We might need to swap this later" is not sufficient justification.
The cost of a premature port is indirection without leverage.

---

## Testing strategy

Old unit tests on shallow pass-through modules become waste
once tests at the deepened module's interface exist.
Delete them.

Tests assert observable behaviour through the interface, not internal state.
A test that reaches into private fields or calls internal helpers
is coupled to the implementation, not the interface.
