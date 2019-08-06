package main

import (
	"fmt"
	"net/http"
)

type HandlerFunc func(http.ResponseWriter, *http.Request) error

func (h HandlerFunc) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if err := h(w, r); err != nil {
		w.WriteHeader(503)
		fmt.Fprintf(w, `{"message": %q}`, err)
	}
}

func Handler(w http.ResponseWriter, r *http.Request) error {
	return fmt.Errorf("hmm")
}

func main() {
	http.ListenAndServe("localhost:8080", HandlerFunc(Handler))
}
