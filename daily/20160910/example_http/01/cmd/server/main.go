package main

import (
	"../.."
	"log"
	"net/http"
)

func main() {
	mux := &http.ServeMux{}
	mux.HandleFunc("/", server.HelloJSON)
	log.Fatal(http.ListenAndServe(":8080", mux))
}
