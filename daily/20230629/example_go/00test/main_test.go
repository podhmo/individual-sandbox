package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"testing"
)

type ctxKey string

const (
	ctxValueKey ctxKey = "value"
)

func Handler(w http.ResponseWriter, req *http.Request) {
	v, ok := req.Context().Value(ctxValueKey).(string)
	fmt.Println("get ", v, ok)
	fmt.Fprintf(w, `{"value": %q, "ok": %t}`, v, ok)
}

func TestIt(t *testing.T) {
	middleware := func(inner http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			inner.ServeHTTP(w, req)
		})
	}
	h := middleware(http.HandlerFunc(Handler))

	rec := httptest.NewRecorder()
	req := httptest.NewRequest("GET", "/", nil)
	h.ServeHTTP(rec, req)
	res := rec.Result()
	if want, got := http.StatusOK, res.StatusCode; want != got {
		t.Fatalf("unexpected status code: want=%d, but got=%d", want, got)
	}
	b, err := httputil.DumpResponse(rec.Result(), true)
	fmt.Println("----------------------------------------")
	fmt.Println(string(b), err)
	fmt.Println("----------------------------------------")
}
