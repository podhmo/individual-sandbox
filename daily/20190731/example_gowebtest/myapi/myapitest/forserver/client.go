package forserver

import (
	"io"
	"myapi/myapitest/interfaces"
	"myapi/myapitest/internal"
	"net/http"
	"net/http/httptest"
	"sync"
)

// Client :
type Client struct {
	client   *http.Client
	Server   *httptest.Server
	BasePath string // need?
}

// Do :
func (c *Client) Do(req *http.Request) (interfaces.Response, error, func()) {
	client := c.client
	if c.client == nil {
		client = http.DefaultClient
	}

	var adapter *internal.ResponseAdapter
	var raw *http.Response
	var once sync.Once

	raw, err := client.Do(req)
	if err != nil {
		return nil, err, nil
	}

	adapter = internal.NewResponseAdapter(
		func() *http.Response {
			once.Do(func() {
				adapter.AddTeardown(raw.Body.Close)
			})
			return raw
		},
	)
	return adapter, err, adapter.Close
}

// Get :
func (c *Client) Get(path string) (interfaces.Response, error, func()) {
	url := internal.URLJoin(c.Server.URL, internal.URLJoin(c.BasePath, path))
	var body io.Reader // xxx (TODO: functional options)
	req, err := http.NewRequest("GET", url, body)
	if err != nil {
		return nil, err, nil
	}
	return c.Do(req)
}
