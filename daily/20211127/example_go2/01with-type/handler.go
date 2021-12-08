package handler

import (
	"encoding/json"
	"io"
	"log"
	"net/http"
)

type Router interface {
	Post(path string, handler http.HandlerFunc)
}

func MountHandle(r Router) {
	type Input struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}
	type Output struct {
		Result Input `json:"result"`
	}

	handle := func(w http.ResponseWriter, req *http.Request) {
		var input Input
		if err := json.NewDecoder(req.Body).Decode(&input); err != nil {
			log.Println(err)
			w.WriteHeader(http.StatusBadRequest)
			io.WriteString(w, http.StatusText(http.StatusBadRequest))
			return
		}
		json.NewEncoder(w).Encode(Output{Result: input})
	}
	r.Post("/", handle)
}
