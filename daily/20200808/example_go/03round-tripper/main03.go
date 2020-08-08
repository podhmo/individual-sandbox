package main

import (
	"bytes"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

type MockTripper struct {
}

func (t *MockTripper) RoundTrip(req *http.Request) (*http.Response, error) {
	header := http.Header{}
	header.Set("Content-Type", "application/json")
	b := []byte(`{"message": "ok"}`)
	return &http.Response{
		Status:        "200 OK",
		StatusCode:    http.StatusOK,
		Proto:         "HTTP/1.0",
		ProtoMajor:    1,
		ProtoMinor:    0,
		Request:       req,
		Header:        header,
		Close:         true,
		ContentLength: int64(len(b)),
		Body:          ioutil.NopCloser(bytes.NewBuffer(b)),
	}, nil
}

func run() error {
	client := &http.Client{
		Transport: &MockTripper{},
	}
	url := os.Getenv("URL")
	res, err := client.Get(url)
	if err != nil {
		return err
	}
	if _, err := io.Copy(os.Stdout, res.Body); err != nil {
		return err
	}
	defer res.Body.Close()
	return nil
}
