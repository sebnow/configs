package main

import (
	"context"
	"log"
)

func main() {
	ctx := context.Background()
	if err := run(ctx); err != nil {
		log.Fatal(err)
	}
}

func run(ctx context.Context) error {
	// TODO: build a *Service and call svc.Run(ctx).
	return nil
}

// Database holds the database connection pool.
type Database struct{}

func NewDatabase() *Database { return &Database{} }

// Cache holds an in-memory key-value cache.
type Cache struct{}

func NewCache() *Cache { return &Cache{} }

// EventBus publishes domain events.
type EventBus struct{}

func NewEventBus() *EventBus { return &EventBus{} }

// Repository persists and reads domain entities.
type Repository struct {
	db *Database
}

func NewRepository(db *Database) *Repository {
	return &Repository{db: db}
}

// Service is the application's primary use case.
type Service struct {
	repo  *Repository
	cache *Cache
	bus   *EventBus
}

func NewService(repo *Repository, cache *Cache, bus *EventBus) *Service {
	return &Service{repo: repo, cache: cache, bus: bus}
}

func (s *Service) Run(ctx context.Context) error { return nil }
