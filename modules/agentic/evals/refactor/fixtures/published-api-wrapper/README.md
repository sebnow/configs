Go package `order` that is shipped as part of the public SDK consumed by external integrators.
The package exposes one entry point: ProcessOrder.
The internal helpers (validate, charge, notify) are unexported.
Breaking changes to ProcessOrder require a major version bump and partner communication.
