package handler

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/podhmo/tenuki"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "hello")
	}))
	defer ts.Close()
	f := tenuki.New(t)
	res := f.Do(f.NewRequest("GET", ts.URL, nil))
	t.Log("response", res)
}
