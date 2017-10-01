package main

import (
	"fmt"
	"net/http"
)

func helloHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello: %s", r.URL.Path[1:])
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/", helloHandler)
	http.ListenAndServe(":8080", mux)
}
