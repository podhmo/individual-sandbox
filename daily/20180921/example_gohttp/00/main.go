package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
)

var val int = 10

func Hello(w http.ResponseWriter, r *http.Request) {
	w.Header().Add("Content-Type", "application/json")
	val++
	encoder := json.NewEncoder(w)
	m := map[string]string{
		"message": fmt.Sprintf("Hello, world! %d\n", val),
	}
	if err := encoder.Encode(m); err != nil {
		w.WriteHeader(500)
		io.WriteString(w, err.Error())
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	mux := &http.ServeMux{}
	mux.HandleFunc("/", Hello)
	server := &http.Server{Addr: ":4444", Handler: mux}
	return server.ListenAndServe()
}
