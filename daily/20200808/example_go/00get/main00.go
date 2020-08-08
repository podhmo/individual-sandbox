package main

import (
	"io"
	"log"
	"net/http"
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
	res, err := client.Get(url)
	if err != nil {
		return err
	}
	if _, err := io.Copy(os.Stdout, res.Body); err != nil {
		return err
	}
	defer res.Body.Close()
	return nil
}
