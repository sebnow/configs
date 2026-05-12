package upstream

import (
	"net/http"
	"net/url"
	"time"
)

// wireUp shows how the upstream client is constructed in production.
// Only *http.Client is ever passed; no test substitutes Doer.
func wireUp() *Client {
	httpClient := &http.Client{Timeout: 5 * time.Second}
	baseURL, _ := url.Parse("https://upstream.example.com")
	return NewClient(httpClient, baseURL)
}
