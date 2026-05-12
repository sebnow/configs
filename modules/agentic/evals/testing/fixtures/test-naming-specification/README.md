A pricing helper with three distinct behaviors that need to be documented by
tests:

- valid price and percent in [0, 100] reduce the price
- negative price is rejected
- percent outside [0, 100] is rejected

There are no tests yet. The task is to add a Go test file that exercises each
behavior. The fixture is intentionally trivial so the names of the tests do
the documenting work — pick names that make the spec readable from
`go test -v` output alone.
