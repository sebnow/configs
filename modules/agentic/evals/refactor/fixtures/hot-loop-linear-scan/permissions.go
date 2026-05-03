package table

var adminRoles = []string{"admin", "superadmin", "site_admin", "root"}

// IsAdmin returns true if the given role is in the admin role list.
func IsAdmin(role string) bool {
	for _, r := range adminRoles {
		if r == role {
			return true
		}
	}
	return false
}
