package postgres

import (
	"context"
	"database/sql"
	"errors"

	"github.com/jackc/pgx/v5/pgconn"
)

var (
	ErrTenantNotFound  = errors.New("tenant not found")
	ErrClientNotFound  = errors.New("client not found")
	ErrDuplicateAPIKey = errors.New("api key already exists")
)

// APIKey is an issued API credential for a tenant.
type APIKey struct {
	ID       string
	TenantID string
	ClientID string
	Hash     []byte
}

// APIKeyRepository persists API keys in Postgres.
//
// Create translates three Postgres error codes to domain sentinel errors:
//   - 23503 / api_key_tenant_id_fkey  -> ErrTenantNotFound
//   - 23503 / api_key_client_id_fkey  -> ErrClientNotFound
//   - 23505 / api_key_hash_key        -> ErrDuplicateAPIKey
//
// The translation is the entire reason this adapter exists; anything above
// the adapter layer only ever sees the sentinel errors.
type APIKeyRepository struct {
	db *sql.DB
}

func NewAPIKeyRepository(db *sql.DB) *APIKeyRepository {
	return &APIKeyRepository{db: db}
}

func (r *APIKeyRepository) Create(ctx context.Context, key APIKey) error {
	_, err := r.db.ExecContext(ctx, `
		INSERT INTO api_key (id, tenant_id, client_id, hash)
		VALUES ($1, $2, $3, $4)
	`, key.ID, key.TenantID, key.ClientID, key.Hash)
	if err == nil {
		return nil
	}

	var pgErr *pgconn.PgError
	if errors.As(err, &pgErr) {
		switch {
		case pgErr.Code == "23503" && pgErr.ConstraintName == "api_key_tenant_id_fkey":
			return ErrTenantNotFound
		case pgErr.Code == "23503" && pgErr.ConstraintName == "api_key_client_id_fkey":
			return ErrClientNotFound
		case pgErr.Code == "23505" && pgErr.ConstraintName == "api_key_hash_key":
			return ErrDuplicateAPIKey
		}
	}
	return err
}
