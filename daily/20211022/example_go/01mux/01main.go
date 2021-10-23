package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
)

func NewGreeting(message string) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		fmt.Fprintf(w, `{"message": %q}`, message)
	}
}

func main() {
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}

	log.Print("listen ...", port)

	mux := new(http.ServeMux)
	mux.Handle("/hello", NewGreeting("hello"))
	mux.Handle("/byebye", NewGreeting("byebye"))

	addr := fmt.Sprintf(":%d", port)
	if err := http.ListenAndServe(addr, mux); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
