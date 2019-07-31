package internal

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"sync"
)

// NewResponseAdapter :
func NewResponseAdapter(get func() *http.Response) *ResponseAdapter {
	return &ResponseAdapter{
		GetResponse: get,
	}
}

// ResponseAdapter :
type ResponseAdapter struct {
	GetResponse func() *http.Response

	bytes []byte // not bet
	bOnce sync.Once

	m         sync.Mutex
	teardowns []func() error
}

// Response :
func (res *ResponseAdapter) Response() *http.Response {
	return res.GetResponse()
}

// Close :
func (res *ResponseAdapter) Close() {
	res.m.Lock()
	defer res.m.Unlock()
	for _, teardown := range res.teardowns {
		if err := teardown(); err != nil {
			panic(err)
		}
	}
	res.teardowns = nil
}

func (res *ResponseAdapter) AddTeardown(fn func() error) {
	res.m.Lock()
	defer res.m.Unlock()
	res.teardowns = append(res.teardowns, fn)
}

// Buffer : (TODO: rename)
func (res *ResponseAdapter) Buffer() *bytes.Buffer {
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
func (res *ResponseAdapter) StatusCode() int {
	return res.Response().StatusCode
}

// ParseData :
func (res *ResponseAdapter) ParseData(val interface{}) error {
	decoder := json.NewDecoder(res.Buffer()) // TODO: decoder interface
	return decoder.Decode(val)
}

// Data :
func (res *ResponseAdapter) Data() interface{} {
	var val interface{}
	if err := res.ParseData(&val); err != nil {
		panic(err) // xxx:
	}
	return val
}

// Body :
func (res *ResponseAdapter) Body() []byte {
	return res.Buffer().Bytes()
}

// LazyBodyString :
func (res *ResponseAdapter) LazyBodyString() fmt.Stringer {
	return NewLazyString(
		func() string {
			return res.Buffer().String()
		},
	)
}
