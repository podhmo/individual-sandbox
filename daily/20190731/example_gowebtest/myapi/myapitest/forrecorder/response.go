package forrecorder

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"myapi/myapitest/internal"
	"net/http"
	"net/http/httptest"
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
func (res *Response) LazyBodyString() fmt.Stringer {
	return internal.NewLazyString(
		func() string {
			return res.Buffer().String()
		},
	)
}
