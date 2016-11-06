package main

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
)

// Hello :
func Hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello world")
}

// Echo :
func Echo(w http.ResponseWriter, r *http.Request) {
	if r.Method != "POST" {
		w.WriteHeader(404)
		return
	}
	if err := r.ParseForm(); err != nil {
		w.WriteHeader(500)
		fmt.Fprintf(w, "%s", err)
	}
	fmt.Fprintf(w, "%s", r.Form.Get("message"))
}

// Echo2 :
func Echo2(w http.ResponseWriter, r *http.Request) {
	reader := io.LimitReader(r.Body, 1024*1024)
	decoder := json.NewDecoder(reader)
	type B struct {
		Message string `json:"message"`
	}
	var b B
	if err := decoder.Decode(&b); err != nil {
		w.WriteHeader(400)
		fmt.Fprintf(w, "%s", err)
	}
	w.Header().Set("Content-Type", "application/json; charset=UTF-8")
	// do something
	if err := json.NewEncoder(w).Encode(&b); err != nil {
		w.WriteHeader(400)
		fmt.Fprintf(w, "%s", err)
	}
}

// NewHandler :
func NewHandler() *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("/", Hello)
	mux.HandleFunc("/echo", Echo)
	mux.HandleFunc("/echo2", Echo2)
	return mux
}

func main() {
	mux := NewHandler()
	http.ListenAndServe(":8080", mux)
}
