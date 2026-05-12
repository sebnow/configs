package db

import (
	"database/sql"

	_ "github.com/lib/pq"
)

// OpenDB returns a connection to the configured Postgres instance.
// It replaces the previously-exported ConnectDB function.
func OpenDB(dsn string) (*sql.DB, error) {
	conn, err := sql.Open("postgres", dsn)
	if err != nil {
		return nil, err
	}
	if err := conn.Ping(); err != nil {
		conn.Close()
		return nil, err
	}
	return conn, nil
}
