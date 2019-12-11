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
	// port, backendServerURL, staticDir
	port := os.Args[1]
	backendServerURL := os.Args[2]
	staticDir := os.Args[3]

	rpURL, err := url.Parse(backendServerURL)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Fprintf(os.Stderr, "start: port=%q, staticDir=%q\n", port, staticDir)

	mux := http.DefaultServeMux
	mux.Handle("/", httputil.NewSingleHostReverseProxy(rpURL))
	mux.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir(staticDir))))
	if err := http.ListenAndServe(fmt.Sprintf(":%s", port), mux); err != nil {
		log.Fatal(err)
	}
}
