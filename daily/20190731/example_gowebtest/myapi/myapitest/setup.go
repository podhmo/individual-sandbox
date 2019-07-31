package myapitest

import (
	"fmt"
	"myapi"
	"net/http"
	"net/http/httptest"
	"strings"

	"github.com/go-chi/chi"
)

// NewTestAPIServer :
func NewTestAPIServer() (*httptest.Server, func()) {
	ts := httptest.NewServer(myapi.Handler())
	return ts, ts.Close
}

// NewTestHandler :
func NewTestHandler() http.HandlerFunc {
	return myapi.Handler().ServeHTTP
}

// NewTestOnlyOneHandler :
func NewTestOnlyOneHandler(pattern, method string, handler http.HandlerFunc) http.HandlerFunc {
	r := chi.NewRouter()
	switch m := strings.ToUpper(method); m {
	case "GET":
		r.Get(pattern, handler)
	default:
		panic(fmt.Sprintf("not supported: %q", m))
	}
	return r.ServeHTTP
}
