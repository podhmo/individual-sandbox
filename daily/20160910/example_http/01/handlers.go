package server

import (
	"encoding/json"
	"log"
	"net/http"
)

type data struct {
	Name    string
	Message string
}

func HelloJSON(w http.ResponseWriter, r *http.Request) {
	data := data{Name: "hello", Message: "hello world"}
	b, err := json.Marshal(data)
	if err != nil {
		log.Fatal(err)
	}
	w.Write(b)
}
