package handler

import (
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/podhmo/tenuki"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		c, _, err := w.(http.Hijacker).Hijack()
		if err != nil {
			t.Fatal(err)
		}
		c.Close()
	}))
	defer ts.Close()

	f := tenuki.New(t)
	req := f.NewRequest("GET", ts.URL, nil)
	res := f.Do(req)
	t.Logf("%+v", res)
}
