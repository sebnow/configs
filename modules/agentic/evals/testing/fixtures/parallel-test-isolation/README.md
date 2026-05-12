User-fetch helper used by an HTTP service.

`FetchUser` reads from the package-global `stubRegistry` map in tests instead
of making a real upstream call. Two test functions — `TestFetchUserHappyPath`
and `TestFetchUserMissing` — both write to and read from `stubRegistry` under
the same `"alice"` key.

The CI build is slower than the team would like, and somebody asked whether
adding `t.Parallel()` to these tests would speed it up. The fixture is the
state of the package right before that change is made: the tests pass under
the default sequential runner, and there is no per-test namespacing of the
shared stub.
