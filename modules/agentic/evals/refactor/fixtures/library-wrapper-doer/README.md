Small Go service that calls an upstream HTTP API.

The package introduces a `Doer` interface and a `Client` struct that wraps
`*http.Client` "to make HTTP calls swappable in tests." Only one production
implementation ever exists (`*http.Client` from the standard library) and no
test substitutes the interface. The `Client` adds no headers, retries,
authentication, or rate-limiting logic — every method forwards directly to
`httpClient.Do`.
