package main

import (
	"context"
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
	fmt.Fprintf(w, `{"value": %q, "ok": %t}`, v, ok)
}

func GetOrSetValue(ctx context.Context, v string) context.Context {
	_, ok := ctx.Value(ctxValueKey).(string)
	if ok {
		return ctx
	}
	return context.WithValue(ctx, ctxValueKey, v)
}

func TestIt(t *testing.T) {
	middleware := func(inner http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			ctx := GetOrSetValue(req.Context(), "MIDDLEWARE")
			req = req.WithContext(ctx)
			inner.ServeHTTP(w, req)
		})
	}
	h := middleware(http.HandlerFunc(Handler))
	// middleware2 := func(inner http.Handler) http.Handler {
	// 	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
	// 		v, _ := req.Context().Value(ctxValueKey).([]string)
	// 		req = req.WithContext(context.WithValue(req.Context(), ctxValueKey, append(v, "MIDDLEWARE2")))
	// 		inner.ServeHTTP(w, req)
	// 	})
	// }
	// h = middleware2(h)

	rec := httptest.NewRecorder()
	req := httptest.NewRequest("GET", "/", nil)
	req = req.WithContext(GetOrSetValue(req.Context(), "REQUEST"))
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
