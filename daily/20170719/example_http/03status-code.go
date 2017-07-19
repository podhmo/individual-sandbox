package main

import (
	"log"
	"net/http"

	"github.com/k0kubun/pp"
)

func run() error {
	response, err := http.Get("https://httpbin.org/status/418")
	if err != nil {
		return err
	}
	defer response.Body.Close()

	type Output struct {
		Code   int    `json:"code"`
		Status string `json:"status"`
	}
	output := Output{Code: response.StatusCode, Status: response.Status}
	pp.Println(&output)
	return nil
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
