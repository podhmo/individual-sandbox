package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"net/http/httptrace"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	client := http.DefaultClient
	url := os.Getenv("URL")

	trace := &httptrace.ClientTrace{
		GetConn: func(hostPort string) {
			fmt.Printf("Get Conn: %+v\n", hostPort)
		},
		GotConn: func(connInfo httptrace.GotConnInfo) {
			fmt.Printf("Got Conn: %+v\n", connInfo)
		},
		GotFirstResponseByte: func() {
			fmt.Println("Got FirstResponseByte")
		},
		DNSStart: func(dnsInfo httptrace.DNSStartInfo) {
			fmt.Printf("DNS Info: %+v\n", dnsInfo)
		},
		DNSDone: func(dnsInfo httptrace.DNSDoneInfo) {
			fmt.Printf("DNS Info: %+v\n", dnsInfo)
		},
		ConnectStart: func(network, addr string) {
			fmt.Printf("Connect Start: %+v %+v\n", network, addr)
		},
		ConnectDone: func(network, addr string, err error) {
			fmt.Printf("Connect Done: %+v %+v %+v\n", network, addr, err)
		},
	}
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return err
	}
	req = req.WithContext(httptrace.WithClientTrace(req.Context(), trace))

	res, err := client.Do(req)

	if err != nil {
		return err
	}
	if _, err := io.Copy(os.Stdout, res.Body); err != nil {
		return err
	}
	defer res.Body.Close()
	return nil
}
