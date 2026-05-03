package users

import (
	"context"
	"database/sql"
	"fmt"
)

type User struct {
	ID    int64
	Email string
	Name  string
}

type UserRepository struct {
	db *sql.DB
}

func NewUserRepository(db *sql.DB) *UserRepository {
	return &UserRepository{db: db}
}

func (r *UserRepository) GetByID(ctx context.Context, id int64) (*User, error) {
	row := r.db.QueryRowContext(ctx, "SELECT id, email, name FROM users WHERE id = $1", id)
	u := &User{}
	if err := row.Scan(&u.ID, &u.Email, &u.Name); err != nil {
		return nil, fmt.Errorf("get user %d: %w", id, err)
	}
	return u, nil
}

func (r *UserRepository) Create(ctx context.Context, email, name string) (*User, error) {
	row := r.db.QueryRowContext(ctx,
		"INSERT INTO users (email, name) VALUES ($1, $2) RETURNING id, email, name",
		email, name,
	)
	u := &User{}
	if err := row.Scan(&u.ID, &u.Email, &u.Name); err != nil {
		return nil, fmt.Errorf("create user: %w", err)
	}
	return u, nil
}

func (r *UserRepository) Update(ctx context.Context, u *User) error {
	_, err := r.db.ExecContext(ctx,
		"UPDATE users SET email = $1, name = $2 WHERE id = $3",
		u.Email, u.Name, u.ID,
	)
	if err != nil {
		return fmt.Errorf("update user %d: %w", u.ID, err)
	}
	return nil
}

func (r *UserRepository) Delete(ctx context.Context, id int64) error {
	_, err := r.db.ExecContext(ctx, "DELETE FROM users WHERE id = $1", id)
	if err != nil {
		return fmt.Errorf("delete user %d: %w", id, err)
	}
	return nil
}
