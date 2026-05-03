Internal Go reporting package with one exported entry point and unexported helpers.
The only consumer is main.go, which is also in this repository.
All callers are visible; the learning-ladder lens must not fire.
The package is not published or shipped to external consumers.
