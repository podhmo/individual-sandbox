package main

import (
	"fmt"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
)

func main() {
	// port, backendServerURL
	port := os.Args[1]
	backendServerURL := os.Args[2]

	rpURL, err := url.Parse(backendServerURL)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Fprintf(os.Stderr, "start: %s\n", port)
	handler := httputil.NewSingleHostReverseProxy(rpURL)
	if err := http.ListenAndServe(fmt.Sprintf(":%s", port), handler); err != nil {
		log.Fatal(err)
	}
}
