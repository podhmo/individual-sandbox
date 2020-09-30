package main

import (
	"net/http/httptest"
	"testing"

	"github.com/go-chi/chi"
	"github.com/podhmo/tenuki"
)

func TestIt(t *testing.T) {
	s := NewServer(chi.NewRouter())
	ts := httptest.NewServer(s)
	defer ts.Close()

	f := tenuki.New(t)
	t.Run("unauthorized", func(t *testing.T) {
		defer f.Capture(t)()
		req := f.NewRequest("GET", ts.URL+"/hello", nil)
		_ = f.Do(req, tenuki.AssertStatus(401))
	})
	t.Run("authorized", func(t *testing.T) {
		defer f.Capture(t)()
		req := f.NewRequest("GET", ts.URL+"/hello", nil)
		req.Header.Add("Authorization", "Bearer ak:user:0")
		_ = f.Do(req, tenuki.AssertStatus(200))
	})
}
