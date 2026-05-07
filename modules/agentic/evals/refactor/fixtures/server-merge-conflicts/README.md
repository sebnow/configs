gRPC API server for a moderation service.

`server.go` holds every handler the service exposes — currently nine handlers
covering four unrelated resources (reports, decisions, rulesets, queue items)
plus the `Server` struct and its repository fields. The file is roughly 800
lines and growing every sprint as new endpoints land.

The team's git history shows the file is a merge-conflict bottleneck: almost
every feature branch touches it, four of the last ten merge commits resolved a
conflict on this file, and the most recent rebase took thirty minutes because
two unrelated PRs both added handlers near the bottom of the file. There is no
file-level coupling between the resources — each handler delegates to its own
repository and shares only the receiver `(s *Server)`.

The handlers themselves are not shallow, the receiver is the right place for
them, and the package layout (one `api` package per service) is deliberate.
The friction is purely operational: too many independent units of work share
one file, so concurrent edits collide.
