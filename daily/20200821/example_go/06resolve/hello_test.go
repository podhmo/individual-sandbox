package main

import (
	"bytes"
	"io"
	"net/http"
	"os"
	"path/filepath"
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

type FS struct {
	http.Dir
	m map[string]string
}

func (d FS) Open(name string) (http.File, error) {
	filename, ok := d.m[name]
	if !ok {
		return nil, os.ErrNotExist
	}
	return d.Dir.Open(filename)
}

func NewFS(root string, m map[string]string) (*FS, error) {
	for _, filename := range m {
		_, err := os.Stat(filepath.Join(root, filename))
		if err != nil {
			return nil, err
		}
	}
	return &FS{
		Dir: http.Dir(root),
		m:   m,
	}, nil
}

func TestIt(t *testing.T) {
	fs, err := NewFS("testdata", map[string]string{
		"/hello": "hello.json",
	})
	if err != nil {
		t.Fatal(err)
	}
	c := &Client{
		HTTPClient: &http.Client{
			Timeout:   100 * time.Millisecond,
			Transport: http.NewFileTransport(fs),
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
