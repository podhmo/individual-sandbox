package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
)

func main() {
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":44444"
	}
	mux := &http.ServeMux{}
	mux.HandleFunc("/hello", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, `{"message": "hello"}`)
	})

	log.Println("listening", addr)
	if err := http.ListenAndServe(addr, mux); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
