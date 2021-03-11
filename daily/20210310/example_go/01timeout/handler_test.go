package handler

import (
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/podhmo/tenuki"
)

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		time.Sleep(200 * time.Millisecond)
	}))
	defer ts.Close()

	f := tenuki.New(t)
	f.Client.Timeout = 100 * time.Millisecond
	req := f.NewRequest("GET", ts.URL, nil)
	res := f.Do(req)
	t.Logf("%+v", res)
}
