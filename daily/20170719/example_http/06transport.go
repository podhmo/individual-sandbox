package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"net/http/httputil"
	"os"

	"github.com/k0kubun/pp"
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

func run() error {
	client := &http.Client{Transport: &DebugTransport{Base: http.DefaultTransport}}
	response, err := client.Get("https://httpbin.org/anything")
	if err != nil {
		return err
	}
	defer response.Body.Close()
	var ob interface{}
	decoder := json.NewDecoder(response.Body)
	if err := decoder.Decode(&ob); err != nil {
		return err
	}
	pp.Println(ob)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
