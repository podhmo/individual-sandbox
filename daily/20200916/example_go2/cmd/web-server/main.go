package main

import (
	"log"
	"m/web"
	"net/http"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	port := os.Getenv("PORT")
	if port == "" {
		port = ":50051"
	}
	log.Println("listening ...", port)

	r := web.NewServer()
	return http.ListenAndServe(port, r)
}
