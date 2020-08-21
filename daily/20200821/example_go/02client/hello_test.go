package main

import (
	"bytes"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

type Client struct {
	BaseURL    string
	HTTPClient *http.Client
}

type RoundTripFunc func(*http.Request) (*http.Response, error)

func (f RoundTripFunc) RoundTrip(r *http.Request) (*http.Response, error) {
	return f(r)
}

func (c *Client) Hello() (*http.Response, error) {
	hc := c.HTTPClient
	if hc == nil {
		hc = http.DefaultClient
	}
	return hc.Get(strings.TrimSuffix(c.BaseURL, "/") + "/hello")
}

func TestIt(t *testing.T) {
	c := &Client{
		HTTPClient: &http.Client{
			Timeout: 100 * time.Millisecond,
			Transport: RoundTripFunc(func(r *http.Request) (*http.Response, error) {
				if r.URL.Path != "/hello" {
					t.Errorf("unexpected path %s", r.URL.Path)
				}
				w := httptest.NewRecorder()
				w.WriteString(`{"message": "hello"}`)
				return w.Result(), nil
			}),
		},
	}
	res, err := c.Hello()
	if err != nil {
		t.Fatal(err)
	}
	defer res.Body.Close()
	var b bytes.Buffer
	io.Copy(&b, res.Body)

	if res.StatusCode != 200 {
		t.Errorf("unexpected: %s", res.Status)
	}

	if b.String() != `{"message": "hello"}` {
		t.Errorf("unmatch: %s", b.String())
	}
}
