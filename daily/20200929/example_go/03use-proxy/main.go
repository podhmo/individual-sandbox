package main

import (
	"crypto/tls"
	"fmt"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"

	"github.com/k0kubun/pp"
	"github.com/podhmo/tenuki"
	"github.com/podhmo/tenuki/capture"
)

func main() {
	defaultTransport := http.DefaultTransport.(*http.Transport)
	transport := &http.Transport{
		Proxy:                 defaultTransport.Proxy,
		DialContext:           defaultTransport.DialContext,
		ForceAttemptHTTP2:     defaultTransport.ForceAttemptHTTP2,
		MaxIdleConns:          defaultTransport.MaxIdleConns,
		IdleConnTimeout:       defaultTransport.IdleConnTimeout,
		ExpectContinueTimeout: defaultTransport.ExpectContinueTimeout,
		TLSHandshakeTimeout:   defaultTransport.TLSHandshakeTimeout,
		TLSClientConfig:       &tls.Config{InsecureSkipVerify: true}, // xxx
	}

	// use httptest.NewServer?
	go func() {
		defer func() {
			fmt.Println("!!!", recover())
		}()
		proxy := &httputil.ReverseProxy{
			Director: func(req *http.Request) {
				pp.Println(req.URL)
				req.URL.Scheme = "https"
				req.URL.Host = "example.net"
			},
			Transport: &capture.CapturedTransport{
				Transport: transport,
				Dumper:    &capture.FileDumper{BaseDir: capture.Dir("proxy")},
			},
		}
		if err := http.ListenAndServeTLS(":8888", "03use-proxy/myself.crt", "03use-proxy/myself.key", proxy); err != nil {
			log.Fatalf("!! %+v", err)
		}
	}()

	client := http.DefaultClient
	client.Transport = tenuki.RoundTripFunc(func(req *http.Request) (*http.Response, error) {
		u := req.URL.String()
		u2, err := url.Parse(fmt.Sprintf("https://localhost:8888/?base=%q", u))
		if err != nil {
			return nil, err
		}

		req.Host = u2.Host
		req.RequestURI = u2.RequestURI()
		req.Header.Set("Content-Type", "application/json")
		req.URL = u2

		return (&capture.CapturedTransport{
			Dumper:    &capture.FileDumper{BaseDir: capture.Dir("original")},
			Transport: transport,
		}).RoundTrip(req)
	})

	fmt.Println(client.Get("https://www.google.co.jp"))
}
