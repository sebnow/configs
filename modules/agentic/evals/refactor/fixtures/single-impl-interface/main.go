package main

import (
	"database/sql"
	"log"

	"example.com/app/adapters"
	"example.com/app/ports"
)

func run(db *sql.DB) {
	var repo ports.UserRepo = adapters.NewPostgresUserRepo(db)
	_ = repo
	log.Println("wired up")
}

func main() {
	db, err := sql.Open("postgres", "postgres://localhost/app")
	if err != nil {
		log.Fatal(err)
	}
	run(db)
}
