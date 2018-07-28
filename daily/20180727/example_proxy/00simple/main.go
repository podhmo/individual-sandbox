package main

import (
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	origin, err := url.Parse("http://localhost:9000/")
	if err != nil {
		return nil
	}

	proxy := &httputil.ReverseProxy{
		Director: func(req *http.Request) {
			req.Header.Add("X-Forwarded-Host", req.Host)
			req.Header.Add("X-Origin-Host", origin.Host)
			// req.Header.Add("X-Origin-Proto", origin.Scheme)
			req.URL.Scheme = "http"
			req.URL.Host = origin.Host
		},
	}

	return http.ListenAndServe(":9001", proxy)
}
