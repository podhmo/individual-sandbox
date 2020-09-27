package main

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"os"
	"testing"
	"time"
)

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
		fmt.Fprintln(os.Stderr, "!! CapturedTransport.T is not found. please use !!")
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

	ct.T.Logf("request: %s", string(b))

	res, err := transport.RoundTrip(req)
	if err != nil {
		return nil, err
	}

	b, err = httputil.DumpResponse(res, true /* body */)
	if err != nil {
		return nil, err
	}
	ct.T.Logf("response: %s", string(b))
	return res, nil
}

func TestIt(t *testing.T) {
	ts := httptest.NewServer(NewHandler())
	defer ts.Close()

	transport := &CapturedTransport{}
	client := &http.Client{
		Timeout:   1 * time.Second,
		Transport: transport,
	}

	t.Run("ng", func(t *testing.T) {
		defer transport.Capture(t)()

		res, err := client.Get(fmt.Sprintf("%s/hello", ts.URL))
		if err != nil {
			t.Fatalf("res: %+v", err)
		}
		defer res.Body.Close()

		if want, got := 401, res.StatusCode; want != got {
			t.Errorf("status code, want %d but %d", want, got)
		}
	})

	t.Run("ok", func(t *testing.T) {
		defer transport.Capture(t)()

		req, err := httptest.NewRequest("GET", fmt.Sprintf("%s/hello", ts.URL), nil)
		if err != nil {
			t.Fatalf("req: %+v", err)
		}
		req.SetBasicAuth("user1", "password1")

		res, err := client.Do(req)
		if err != nil {
			t.Fatalf("res: %+v", err)
		}
		defer res.Body.Close()

		if want, got := 200, res.StatusCode; want != got {
			t.Errorf("status code, want %d but %d", want, got)
		}
	})
}
