package main

import (
	"bytes"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httputil"
	"os"
	"strconv"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	c := &http.Client{Transport: NewDebugTransport()}
	req, err := http.NewRequest("GET", "http://localhost:8080", nil)
	if err != nil {
		return err
	}
	res, err := c.Do(req)
	if err != nil {
		return err
	}
	io.Copy(os.Stderr, res.Body)
	return nil
}

func NewDebugTransport() *DebugTransport {
	t := &DebugTransport{}
	if t.RoundTripper == nil {
		t.RoundTripper = http.DefaultTransport
	}
	if t.Writer == nil {
		t.Writer = os.Stderr
	}
	return t
}

// DebugTrasport :
type DebugTransport struct {
	IgnoreDumpRequest  bool
	IgnoreDumpResponse bool
	Quiet              bool

	Writer       io.Writer
	RoundTripper http.RoundTripper
}

// RoundTrip :
func (t *DebugTransport) RoundTrip(req *http.Request) (resp *http.Response, err error) {
	if !t.IgnoreDumpRequest {
		b, err := httputil.DumpRequest(req, !t.Quiet)
		if err != nil {
			return nil, err
		}
		if _, err := t.Writer.Write(b); err != nil {
			return nil, err
		}
	}

	resp, err = t.RoundTripper.RoundTrip(req)
	if err != nil {
		return nil, err
	}

	defer resp.Body.Close()
	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	b = bytes.Replace(b, []byte("server"), []byte("schmerver"), -1)
	body := ioutil.NopCloser(bytes.NewReader(b))
	resp.ContentLength = int64(len(b))
	resp.Header.Set("Content-Length", strconv.Itoa(len(b)))
	resp.Body = body

	if !t.IgnoreDumpResponse {
		b, err := httputil.DumpResponse(resp, !t.Quiet)
		if err != nil {
			return nil, err
		}
		if _, err := t.Writer.Write(b); err != nil {
			return nil, err
		}
	}
	return resp, nil
}
