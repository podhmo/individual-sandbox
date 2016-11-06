package main

import (
	"fmt"
	"net/http"
)

// Hello :
func Hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello world")
}

// NewHandler :
func NewHandler() *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("/", Hello)
	return mux
}

func main() {
	mux := NewHandler()
	http.ListenAndServe(":8080", mux)
}
