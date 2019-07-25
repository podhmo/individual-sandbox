package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	url := os.Args[1]
	if err := run(url); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(url string) error {
	res, err := http.Get(url)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	io.Copy(os.Stdout, res.Body)
	return nil
}
