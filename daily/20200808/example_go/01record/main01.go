package main

import (
	"io"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/dnaeon/go-vcr/cassette"
	"github.com/dnaeon/go-vcr/recorder"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	r, err := recorder.New("fixtures/01")
	if err != nil {
		return err
	}
	defer r.Stop()
	r.AddFilter(func(i *cassette.Interaction) error {
		delete(i.Request.Headers, "Authorization")
		return nil
	})
	r.AddPassthrough(func(req *http.Request) bool {
		return req.URL.Path == "/login"
	})

	client := &http.Client{
		Transport: r,
		Timeout:   3 * time.Second,
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
