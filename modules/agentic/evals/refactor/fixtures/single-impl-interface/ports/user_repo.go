package ports

import "context"

type User struct {
	ID    int64
	Email string
	Name  string
}

// UserRepo is the persistence port for user records.
type UserRepo interface {
	GetByID(ctx context.Context, id int64) (*User, error)
	Create(ctx context.Context, email, name string) (*User, error)
	Update(ctx context.Context, u *User) error
	Delete(ctx context.Context, id int64) error
}
