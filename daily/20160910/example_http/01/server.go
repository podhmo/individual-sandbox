package main

import (
	"encoding/json"
	"log"
	"net/http"
)

type data struct {
	Name    string
	Message string
}

func main() {
	mux := &http.ServeMux{}
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		data := data{Name: "hello", Message: "hello world"}
		b, err := json.Marshal(data)
		if err != nil {
			log.Fatal(err)
		}
		w.Write(b)
	})
	log.Fatal(http.ListenAndServe(":8080", mux))
}
