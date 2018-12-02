package main

import (
	"fmt"
	"log"
	"net/http"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	// // xxx:
	// http.DefaultTransport.(*http.Transport).TLSClientConfig = &tls.Config{InsecureSkipVerify: true}

	res, err := http.Get("https://example.com")
	if err != nil {
		return err
	}
	defer res.Body.Close()
	fmt.Println(res.Status, res.Request.URL.String())
	return nil
}
