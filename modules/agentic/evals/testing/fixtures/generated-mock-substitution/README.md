Activity layer of a moderation service.

`IssueAPIKey` depends on an `APIKeyRepository` interface so the database can be
substituted in tests. There is no test for `IssueAPIKey` yet and no
substitution mechanism exists.

The task is to add a unit test for `IssueAPIKey` that exercises the error path
(repository returns an error) and the happy path. The test must substitute the
repository — there is no Postgres available in this fixture.

The repo already uses `go tool` directives for code generation: `go tool moq`
is available without further setup.
