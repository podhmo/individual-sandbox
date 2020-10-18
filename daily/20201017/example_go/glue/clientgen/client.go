package clientgen

import (
	"net/http"
	"strings"
)

type Client struct {
	BaseURL string
	*Driver
	*Decoder
}

func NewClient(baseURL string, options ...func(*Client)) *Client {
	client := &Client{
		BaseURL: strings.TrimSuffix(baseURL, "/"),
		Driver:  NewDriver(),
		Decoder: NewDecoder(),
	}
	for _, opt := range options {
		opt(client)
	}
	return client
}

func WithHTTPClient(httpClient *http.Client) func(*Client) {
	return func(c *Client) {
		c.HTTPClient = httpClient
	}
}
