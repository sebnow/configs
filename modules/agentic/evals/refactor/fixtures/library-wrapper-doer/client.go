package upstream

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
)

// Doer abstracts the HTTP transport so we could swap implementations later.
type Doer interface {
	Do(*http.Request) (*http.Response, error)
}

// Client talks to the upstream service.
type Client struct {
	httpClient Doer
	baseURL    *url.URL
}

func NewClient(httpClient Doer, baseURL *url.URL) *Client {
	return &Client{httpClient: httpClient, baseURL: baseURL}
}

func (c *Client) GetWidget(ctx context.Context, id string) (*Widget, error) {
	u := c.baseURL.JoinPath("widgets", id)
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, u.String(), nil)
	if err != nil {
		return nil, err
	}
	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected status: %d", resp.StatusCode)
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}
	var w Widget
	if err := json.Unmarshal(body, &w); err != nil {
		return nil, err
	}
	return &w, nil
}

type Widget struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}
