// +build !release

package main

import (
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
)

func init() {
	u, err := url.Parse("http://localhost:3000/")
	if err != nil {
		log.Fatal(err)
	}
	http.Handle("/", httputil.NewSingleHostReverseProxy(u))
}
