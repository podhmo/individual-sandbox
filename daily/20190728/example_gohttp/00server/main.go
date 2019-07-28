package main

import (
	"encoding/json"
	"log"
	"net/http"
	"time"
)

type greeting struct {
	Message string    `json:"message"`
	Now     time.Time `json:"now"`
}

func Greeting(w http.ResponseWriter, r *http.Request) {
	r.Header.Set("Content-Type", "application/json")
	encoder := json.NewEncoder(w)
	encoder.SetIndent("", "  ")
	g := &greeting{
		Message: "hello world",
		Now:     time.Now(),
	}
	if err := encoder.Encode(g); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func main() {
	mux := http.NewServeMux()
	mux.HandleFunc("/", Greeting)
	log.Fatal(http.ListenAndServe(":8080", mux))
}
