package main

import (
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
	"github.com/k0kubun/pp"
)

func Router() chi.Router {
	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Get("/api/{message}", func(w http.ResponseWriter, r *http.Request) {
		data := map[string]string{
			"message": "hello",
		}
		cc := chi.RouteContext(r.Context())
		// todo: cookie, headers, forms
		pp.Println("url params", cc.URLParams)
		pp.Println("form params", r.Form)
		pp.Println("query params", r.URL.Query())
		pp.Println("headers", r.Header)
		render.JSON(w, r, data)
	})
	return r
}

func TestIt(t *testing.T) {
	// TODO: use path and query

	ts := httptest.NewServer(Router())
	resp, err := http.Get(fmt.Sprintf("%s/api/hello?pretty=1", ts.URL))
	if err != nil {
		t.Fatalf("!! %+v", err)
	}
	io.Copy(os.Stdout, resp.Body)
	defer resp.Body.Close()
}
