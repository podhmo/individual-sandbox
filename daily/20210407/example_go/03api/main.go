package main

import (
	"context"
	"fmt"
	"log"
	"net/http"

	"github.com/getkin/kin-openapi/openapi3filter"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	// これはresponseのvalidationが効いていないらしい
	handler := &openapi3filter.ValidationHandler{
		Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			fmt.Fprintf(w, `{"message": "hello world"}`)
		}),
		AuthenticationFunc: func(context.Context, *openapi3filter.AuthenticationInput) error {
			return nil
		},
		SwaggerFile:  "openapi.yaml",
		ErrorEncoder: openapi3filter.DefaultErrorEncoder, // todo: application/json
	}
	if err := handler.Load(); err != nil {
		return err
	}

	addr := ":8888"
	log.Println("listen ...", addr)
	http.ListenAndServe(addr, handler)
	return nil
}
