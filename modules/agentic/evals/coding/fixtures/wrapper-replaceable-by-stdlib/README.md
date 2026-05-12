A small Go package handling signup and email-change flows.

`signup.validateEmail` is an unexported helper that does a few
syntactic checks on email strings. It is called from `Signup` and
`ChangeEmail`.

The Go standard library's `net/mail.ParseAddress` parses email
addresses to RFC 5322 — covers the same checks and more. The team
has decided to use the stdlib parser for email format validation
going forward.
