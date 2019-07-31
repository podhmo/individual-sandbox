package forrecorder

import (
	"io"
	"myapi/myapitest/interfaces"
	"myapi/myapitest/internal"
	"net/http"
	"net/http/httptest"
)

// Client :
type Client struct {
	HandlerFunc http.HandlerFunc
	BasePath    string
}

// Do :
func (c *Client) Do(req *http.Request) (interfaces.Response, func()) {
	w := httptest.NewRecorder()
	c.HandlerFunc(w, req)
	res := &Response{recorder: w}
	return res, res.Close
}

// Get :
func (c *Client) Get(path string) (interfaces.Response, func()) {
	url := internal.URLJoin(c.BasePath, path)
	var body io.Reader // xxx (TODO: functional options)
	req := httptest.NewRequest("GET", url, body)
	return c.Do(req)
}
