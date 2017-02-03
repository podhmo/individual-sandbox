package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httputil"
	"os"

	"github.com/davecgh/go-spew/spew"
)

// DebugTransport :
type DebugTransport struct {
	Base    http.RoundTripper
	Verbose bool
}

// RoundTrip :
func (t *DebugTransport) RoundTrip(request *http.Request) (*http.Response, error) {
	b, err := httputil.DumpRequest(request, t.Verbose)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(os.Stderr, string(b))

	response, err := t.Base.RoundTrip(request)
	if err != nil {
		return nil, err
	}
	b2, err := httputil.DumpResponse(response, t.Verbose)
	if err != nil {
		return nil, err
	}
	fmt.Fprintln(os.Stderr, string(b2))
	return response, err
}

// Result :
type Result struct {
	URL       string `json:"url"`
	Title     string `json:"title"`
	UpdatedAt string `json:"updated_at"`
}

func get() error {
	client := &http.Client{Transport: &DebugTransport{Base: http.DefaultTransport}}
	resp, err := client.Get("http://qiita.com/api/v1/search?q=go")
	if err != nil {
		return err
	}
	decoder := json.NewDecoder(resp.Body)

	var xs []Result
	if err := decoder.Decode(&xs); err != nil {
		return err
	}
	spew.Dump(xs)
	defer resp.Body.Close()
	return nil
}

func main() {
	get()
}
