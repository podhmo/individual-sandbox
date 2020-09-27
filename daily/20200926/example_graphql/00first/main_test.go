package main

import (
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"os"
	"strings"
	"testing"
)

func TestIt(t *testing.T) {
	h, _ := NewHandler()
	ts := httptest.NewServer(h)
	defer ts.Close()

	req, err := http.NewRequest("POST", fmt.Sprintf("%s/graphql", ts.URL),
		strings.NewReader(strings.ReplaceAll(`
{
"query": "
query {
  post(id: 5) {
    userId
    id
    body
    title
    comments {
      id
      email
      name
    }
  }
}
"}
`, "\n", "")),
	)
	if err != nil {
		t.Fatalf("!! req: %+v", err)
	}
	client := &http.Client{
		Transport: &CapturedTransport{T: t},
	}
	res, err := client.Do(req)
	if err != nil {
		t.Fatalf("!! res: %+v", err)
	}

	io.Copy(os.Stdout, res.Body)
}

type CapturedTransport struct {
	Transport http.RoundTripper
	T         *testing.T
}

func (ct *CapturedTransport) Capture(t *testing.T) func() {
	ct.T = t
	return func() {
		ct.T = nil
	}
}

func (ct *CapturedTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	if ct.T == nil {
		fmt.Fprintln(os.Stderr, "!! CapturedTransport.T is not found !!")
		fmt.Fprintln(os.Stderr, "please use `defer transport.Capture(t)()`")
	}

	transport := ct.Transport
	if transport == nil {
		transport = http.DefaultTransport
	}

	b, err := httputil.DumpRequest(req, true /* body */)
	if err != nil {
		return nil, err
	}

	ct.T.Logf("\x1b[5G\x1b[0K\x1b[90mrequest:\n%s\x1b[0m", string(b))

	res, err := transport.RoundTrip(req)
	if err != nil {
		return nil, err
	}

	b, err = httputil.DumpResponse(res, true /* body */)
	if err != nil {
		return nil, err
	}
	ct.T.Logf("\x1b[5G\x1b[0K\x1b[90mresponse:\n%s\x1b[0m", string(b))
	return res, nil
}
