package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

// Transport :
type Transport struct {
	Base http.RoundTripper
}

// RoundTrip :
func (t *Transport) RoundTrip(request *http.Request) (*http.Response, error) {
	// pp.Print(request.URL)
	return t.Base.RoundTrip(request)
}

func tick(client *http.Client, url string) {
	response, err := client.Get(url)
	if err != nil {
		log.Fatal(err)
	}
	io.Copy(os.Stdout, response.Body)
}

func main() {
	client := &http.Client{Transport: &Transport{Base: http.DefaultTransport}}
	tick(client, "http://localhost:54321/foo")
	tick(client, "http://localhost:54321/foo")
	tick(client, "http://localhost:54321/bar")
}
