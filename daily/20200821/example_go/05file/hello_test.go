package main

import (
	"bytes"
	"io"
	"net/http"
	"strings"
	"testing"
	"time"
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

type Dir struct {
	http.Dir
	Ext string
}

func (d Dir) Open(name string) (http.File, error) {
	return d.Dir.Open(name + d.Ext)
}

func TestIt(t *testing.T) {
	c := &Client{
		HTTPClient: &http.Client{
			Timeout:   100 * time.Millisecond,
			Transport: http.NewFileTransport(Dir{Dir: http.Dir("testdata"), Ext: ".json"}),
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

	if strings.TrimSpace(b.String()) != strings.TrimSpace(`{"message": "hello"}`) {
		t.Errorf("unmatch: %s", b.String())
	}
}
