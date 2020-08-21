package main

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

type Client struct {
	BaseURL    string
	HTTPClient *http.Client
}

func (c *Client) Hello() (*http.Response, error) {
	hc := c.HTTPClient
	if hc == nil {
		hc = http.DefaultClient
	}
	return hc.Get(strings.TrimSuffix(c.BaseURL, "/") + "/hello")
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, `{"message": "hello"}`)
	}))
	defer ts.Close()

	c := &Client{
		BaseURL: ts.URL,
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
