Postgres adapter for the API key repository.

`APIKeyRepository.Create` issues an INSERT and translates three Postgres
error codes — two foreign-key violations and one unique-key violation — into
domain sentinel errors (`ErrTenantNotFound`, `ErrClientNotFound`,
`ErrDuplicateAPIKey`). The translation depends on inspecting the SQLSTATE
code and constraint name carried by `pgconn.PgError` values produced by the
Postgres driver in response to actual schema constraints.

There is no test for `Create` yet. The repository is the only thing in this
fixture; activities and handlers that depend on it live elsewhere in the
service.
