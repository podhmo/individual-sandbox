package handler

import (
	"encoding/json"
	"io"
	"log"
	"net/http"
)

func Handle(w http.ResponseWriter, req *http.Request) {
	type Input struct {
		Name string `json:"name"`
		Age  int    `json:"age"`
	}

	var input Input
	if err := json.NewDecoder(req.Body).Decode(&input); err != nil {
		log.Println(err)
		w.WriteHeader(http.StatusBadRequest)
		io.WriteString(w, http.StatusText(http.StatusBadRequest))
		return
	}

	type Output struct {
		Result Input `json:"result"`
	}
	json.NewEncoder(w).Encode(Output{Result: input})
}
