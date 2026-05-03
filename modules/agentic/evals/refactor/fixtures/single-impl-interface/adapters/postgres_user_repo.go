package adapters

import (
	"context"
	"database/sql"
	"fmt"

	"example.com/app/ports"
)

type PostgresUserRepo struct {
	db *sql.DB
}

func NewPostgresUserRepo(db *sql.DB) *PostgresUserRepo {
	return &PostgresUserRepo{db: db}
}

func (r *PostgresUserRepo) GetByID(ctx context.Context, id int64) (*ports.User, error) {
	row := r.db.QueryRowContext(ctx, "SELECT id, email, name FROM users WHERE id = $1", id)
	u := &ports.User{}
	if err := row.Scan(&u.ID, &u.Email, &u.Name); err != nil {
		return nil, fmt.Errorf("get user %d: %w", id, err)
	}
	return u, nil
}

func (r *PostgresUserRepo) Create(ctx context.Context, email, name string) (*ports.User, error) {
	row := r.db.QueryRowContext(ctx,
		"INSERT INTO users (email, name) VALUES ($1, $2) RETURNING id, email, name",
		email, name,
	)
	u := &ports.User{}
	if err := row.Scan(&u.ID, &u.Email, &u.Name); err != nil {
		return nil, fmt.Errorf("create user: %w", err)
	}
	return u, nil
}

func (r *PostgresUserRepo) Update(ctx context.Context, u *ports.User) error {
	_, err := r.db.ExecContext(ctx,
		"UPDATE users SET email = $1, name = $2 WHERE id = $3",
		u.Email, u.Name, u.ID,
	)
	return err
}

func (r *PostgresUserRepo) Delete(ctx context.Context, id int64) error {
	_, err := r.db.ExecContext(ctx, "DELETE FROM users WHERE id = $1", id)
	return err
}
