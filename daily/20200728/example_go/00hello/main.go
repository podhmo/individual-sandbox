package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
)

func NewHandler() http.Handler {
	mux := http.NewServeMux()
	mux.Handle("/hello", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"message": "hello world"}`)
	}))
	return mux
}

func main() {
	h := NewHandler()
	log.Fatal(http.ListenAndServe(os.Getenv("ADDR"), h))
}
