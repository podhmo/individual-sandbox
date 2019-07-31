package forrecorder

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
	HandlerFunc http.HandlerFunc
	BasePath    string
}

// Do :
func (c *Client) Do(req *http.Request) (interfaces.Response, error, func()) {
	var adapter *internal.ResponseAdapter
	var raw *http.Response
	var once sync.Once

	w := httptest.NewRecorder()
	c.HandlerFunc(w, req)

	adapter = internal.NewResponseAdapter(
		func() *http.Response {
			once.Do(func() {
				raw = w.Result()
				adapter.AddTeardown(raw.Body.Close)
			})
			return raw
		},
	)
	return adapter, nil, adapter.Close
}

// Get :
func (c *Client) Get(path string) (interfaces.Response, error, func()) {
	url := internal.URLJoin(c.BasePath, path)
	var body io.Reader // xxx (TODO: functional options)
	req := httptest.NewRequest("GET", url, body)
	return c.Do(req)
}
