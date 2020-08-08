package main

import (
	"context"
	"io"
	"log"
	"m/generated"
	"net/http"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!%+v", err)
	}
}

func run() error {
	c := generated.NewConfiguration()
	c.HTTPClient = http.DefaultClient
	client := generated.NewAPIClient(c)

	ctx := context.Background()
	_, res, err := client.DefaultApi.GetHeaders(ctx)
	if err != nil {
		return err
	}
	if _, err := io.Copy(os.Stdout, res.Body); err != nil {
		return err
	}
	defer res.Body.Close()
	return nil
}
