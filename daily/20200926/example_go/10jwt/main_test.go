package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"strings"
	"testing"
	"time"
)

type TripperFunc func(*http.Request) (*http.Response, error)

func (f TripperFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(NewHandler())
	defer ts.Close()
	var currentT *testing.T
	c := &http.Client{
		Transport: TripperFunc(func(r *http.Request) (*http.Response, error) {
		}),
		Timeout: 1 * time.Second,
	}

	// POST /signin
	var state []*http.Cookie
	t.Run("signin, ok", func(t *testing.T) {
		currentT = t
		res, err := c.Post(
			fmt.Sprintf("%s/signin", ts.URL),
			"application/json",
			strings.NewReader(`
{"username": "user1", "password": "password1"}
`))
		if err != nil {
			t.Fatalf("unexpected /sigin %+v", err)
		}
		if want, got := 200, res.StatusCode; got != want {
			t.Fatalf("want %d, but %d", want, got)
		}

		state = res.Cookies()
		if want, got := 1, len(state); got != want {
			t.Errorf("cookie length, want %d, but %d", want, got)
			t.Logf("cookies: %+v", state)
		}
	})

	t.Run("welcome,ok", func(t *testing.T) {
		currentT = t

		req, err := http.NewRequest("GET", fmt.Sprintf("%s/welcome", ts.URL), nil)
		if err != nil {
			t.Fatalf("!%+v", err)
		}
		for i := range state {
			req.AddCookie(state[i])
		}
		res, err := c.Do(req)
		if err != nil {
			t.Fatalf("unexpected /welcome %+v", err)
		}
		if want, got := 200, res.StatusCode; got != want {
			t.Fatalf("want %d, but %d", want, got)
		}
	})
	t.Run("welcome,ng", func(t *testing.T) {
		currentT = t

		req, err := http.NewRequest("GET", fmt.Sprintf("%s/welcome", ts.URL), nil)
		if err != nil {
			t.Fatalf("!%+v", err)
		}
		res, err := c.Do(req)
		if err != nil {
			t.Fatalf("unexpected /welcome %+v", err)
		}
		if want, got := 401, res.StatusCode; got != want {
			t.Fatalf("want %d, but %d", want, got)
		}
	})

	// t.Run("refresh,ok", func(t *testing.T) {
	// 	currentT = t
	// 	req, err := http.NewRequest("POST", fmt.Sprintf("%s/refresh", ts.URL), nil)
	// 	if err != nil {
	// 		t.Fatalf("!%+v", err)
	// 	}
	// 	for i := range state {
	// 		req.AddCookie(state[i])
	// 	}
	// 	res, err := c.Do(req)
	// 	if err != nil {
	// 		t.Fatalf("unexpected /refresh %+v", err)
	// 	}
	// 	if want, got := 200, res.StatusCode; got != want {
	// 		t.Fatalf("want %d, but %d", want, got)
	// 	}
	// })
}
