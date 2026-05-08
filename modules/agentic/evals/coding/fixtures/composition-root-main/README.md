Application entry-point fixture.

`main.go` declares the dependency types (Database, Cache, EventBus,
Repository, Service) and their constructors. The `run` function is the
composition root; its body is a stub. The task is to wire the
dependencies and invoke `Service.Run`.
