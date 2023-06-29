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
	ctxValueKey  ctxKey = "value"
	ctxInjectKey ctxKey = "inject"
)

func Handler(w http.ResponseWriter, req *http.Request) {
	v, ok := req.Context().Value(ctxValueKey).([]string)
	fmt.Fprintf(w, `{"value": %q, "ok": %t}`, v, ok)
}

func InjectComponentsMiddlware(hooks ...func(context.Context) context.Context) func(http.Handler) http.Handler {
	return func(inner http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
			ctx := req.Context()
			for _, inject := range hooks {
				ctx = inject(ctx)
			}
			innerHooks, ok := ctx.Value(ctxInjectKey).([]func(context.Context) context.Context)
			if ok {
				for _, inject := range innerHooks {
					ctx = inject(ctx)
				}
			}
			req = req.WithContext(ctx)
			inner.ServeHTTP(w, req)
		})
	}
}

func InjectComponentsFunction(req *http.Request, hooks ...func(context.Context) context.Context) *http.Request {
	ctx := req.Context()
	innerHooks, _ := ctx.Value(ctxInjectKey).([]func(context.Context) context.Context)
	return req.WithContext(context.WithValue(ctx, ctxInjectKey, append(innerHooks, hooks...)))
}

func TestIt(t *testing.T) {
	middleware := InjectComponentsMiddlware(func(ctx context.Context) context.Context {
		v, _ := ctx.Value(ctxValueKey).([]string)
		return context.WithValue(ctx, ctxValueKey, append(v, "MIDDLEWARE"))
	})
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
	req = InjectComponentsFunction(req, func(ctx context.Context) context.Context {
		v, _ := ctx.Value(ctxValueKey).([]string)
		return context.WithValue(ctx, ctxValueKey, append(v, "REQUEST"))
	})
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
