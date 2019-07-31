package myapitest

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
)

// Response :
type Response struct {
	recorder *httptest.ResponseRecorder

	bytes []byte // not bet
	bOnce sync.Once

	response *http.Response
	rOnce    sync.Once

	m         sync.Mutex
	teardowns []func() error
}

// Close :
func (res *Response) Close() {
	res.m.Lock()
	defer res.m.Unlock()
	for _, teardown := range res.teardowns {
		if err := teardown(); err != nil {
			panic(err)
		}
	}
	res.teardowns = nil
}

func (res *Response) addTeardown(fn func() error) {
	res.m.Lock()
	defer res.m.Unlock()
	res.teardowns = append(res.teardowns, fn)
}

// Response :
func (res *Response) Response() *http.Response {
	res.rOnce.Do(func() {
		res.response = res.recorder.Result()
		res.addTeardown(res.response.Body.Close)
	})
	return res.response
}

// Buffer : (TODO: rename)
func (res *Response) Buffer() *bytes.Buffer {
	res.bOnce.Do(func() {
		var b bytes.Buffer
		if _, err := io.Copy(&b, res.Response().Body); err != nil {
			panic(err) // xxx
		}
		res.bytes = b.Bytes()
	})
	return bytes.NewBuffer(res.bytes)
}

// StatusCode :
func (res *Response) StatusCode() int {
	return res.Response().StatusCode
}

// ParseData :
func (res *Response) ParseData(val interface{}) error {
	decoder := json.NewDecoder(res.Buffer()) // TODO: decoder interface
	return decoder.Decode(val)
}

// Data :
func (res *Response) Data() interface{} {
	var val interface{}
	if err := res.ParseData(&val); err != nil {
		panic(err) // xxx:
	}
	return val
}

// Body :
func (res *Response) Body() []byte {
	return res.Buffer().Bytes()
}

// LazyBodyString :
func (res *Response) LazyBodyString() *LazyString {
	return &LazyString{
		ToString: func() string {
			return res.Buffer().String()
		},
	}
}

// Client :
type Client struct {
	HandlerFunc http.HandlerFunc
	BasePath    string
}

// Do :
func (c *Client) Do(req *http.Request) *Response {
	w := httptest.NewRecorder()
	c.HandlerFunc(w, req)
	return &Response{recorder: w}
}

// Get :
func (c *Client) Get(path string) (*Response, func()) {
	url := urljoin(c.BasePath, path)
	var body io.Reader // xxx (TODO: functional options)
	req := httptest.NewRequest("GET", url, body)
	res := c.Do(req)
	return res, res.Close
}

// ----------------------------------------
// utilitties
// ----------------------------------------

func urljoin(x, y string) string {
	if x == "" {
		return y
	}
	if y == "" {
		return x
	}
	return fmt.Sprintf("%s/%s", strings.TrimSuffix(x, "/"), strings.TrimPrefix(y, "/"))
}

// LazyString :
type LazyString struct {
	ToString func() string

	result string
	once   sync.Once
}

// String
func (ls *LazyString) String() string {
	ls.once.Do(func() {
		ls.result = ls.ToString()
	})
	return ls.result
}
