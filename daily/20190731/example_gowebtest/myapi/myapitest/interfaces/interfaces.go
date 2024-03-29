package interfaces

import (
	"fmt"
	"net/http"
)

// Client :
type Client interface {
	Do(req *http.Request) (Response, error, func())
	Get(path string) (Response, error, func())
}

// Response :
type Response interface {
	Close()

	Response() *http.Response
	StatusCode() int

	Extractor
}

// Extractor :
type Extractor interface {
	ParseData(val interface{}) error
	Data() interface{}

	Body() []byte
	LazyBodyString() fmt.Stringer
}
