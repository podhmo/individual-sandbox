package main

import (
	"encoding/json"
	"log"
	"net/http"
)

func main() {
	addr := ":8008"
	log.Println("listen ...", addr)
	http.ListenAndServe(addr, http.HandlerFunc(handler))
}

type Message struct {
	Message string `json:"message"`
}

func handler(w http.ResponseWriter, req *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	enc := json.NewEncoder(w)
	enc.Encode(Message{"hello"})
}
