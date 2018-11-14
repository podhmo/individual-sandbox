package main

import (
	"log"
	"net/http"

	internal "./internal"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	srv := internal.NewServer()
	return http.ListenAndServe(":8080", srv.Handler())
}
