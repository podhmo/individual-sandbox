package main

import (
	"context"
	"io"
	"log"
	"m/generated2"
	"net/http"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	client, err := generated2.NewClient(
		"https://httpbin.org/",
		generated2.WithHTTPClient(http.DefaultClient),
	)
	if err != nil {
		return err
	}

	ctx := context.Background()
	res, err := client.GetHeaders(ctx)
	if err != nil {
		return err
	}
	if _, err := io.Copy(os.Stdout, res.Body); err != nil {
		return err
	}
	defer res.Body.Close()
	return nil
}
