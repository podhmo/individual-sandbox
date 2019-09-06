package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"net/http/httputil"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	c := &http.Client{Transport: &DebugTransport{}}
	req, err := http.NewRequest("GET", "http://localhost:8080", nil)
	if err != nil {
		return err
	}
	res, err := c.Do(req)
	if err != nil {
		return err
	}

	fmt.Println("----------------------------------------")
	httputil.DumpResponse(res, true)
	io.Copy(os.Stderr, res.Body)
	fmt.Println(res.StatusCode)
	fmt.Println("----------------------------------------")
	return nil
}

// DebugTransport :
type DebugTransport struct {
	IgnoreDumpRequest  bool
	IgnoreDumpResponse bool
	Quiet              bool

	Writer    io.Writer
	Transport http.RoundTripper
}

// transport :
func (d *DebugTransport) transport() http.RoundTripper {
	if d.Transport != nil {
		return d.Transport
	}
	return http.DefaultTransport
}

// writer :
func (d *DebugTransport) writer() io.Writer {
	if d.Writer != nil {
		return d.Writer
	}
	return os.Stderr
}

// RoundTrip :
func (t *DebugTransport) RoundTrip(req *http.Request) (resp *http.Response, err error) {
	if !t.IgnoreDumpRequest {
		b, err := httputil.DumpRequest(req, !t.Quiet)
		if err != nil {
			return nil, err
		}
		if _, err := t.writer().Write(b); err != nil {
			return nil, err
		}
	}

	resp, err = t.transport().RoundTrip(req)
	if err != nil {
		return nil, err
	}

	if !t.IgnoreDumpResponse {
		b, err := httputil.DumpResponse(resp, !t.Quiet)
		if err != nil {
			return nil, err
		}
		if _, err := t.writer().Write(b); err != nil {
			return nil, err
		}
	}
	return resp, nil
}
