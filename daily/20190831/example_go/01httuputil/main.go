package main

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httputil"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

func run() error {
	c := &http.Client{}
	req, err := http.NewRequest("GET", "http://localhost:8080", nil)
	if err != nil {
		return err
	}
	res, err := c.Do(req)
	if err != nil {
		return err
	}
	var copied http.Response
	copied = *res
	var buf bytes.Buffer

	res.Body = ioutil.NopCloser(io.TeeReader(res.Body, &buf))
	copied.Body = ioutil.NopCloser(&buf)

	b, err := httputil.DumpResponse(res, true)
	if err != nil {
		return err
	}
	fmt.Println(string(b))
	fmt.Println("----------------------------------------")
	io.Copy(os.Stderr, copied.Body)
	return nil
}
