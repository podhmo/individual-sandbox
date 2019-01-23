package handler

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
)

// HelloInput :
type HelloInput struct {
	Name string `json:"name"`
}

// HelloOutput :
type HelloOutput struct {
	Message string `json:"message"`
}

// Hello :
func Hello(w http.ResponseWriter, r *http.Request) {
	decoder := json.NewDecoder(r.Body)
	var input HelloInput
	if err := decoder.Decode(&input); err != nil {
		w.WriteHeader(http.StatusBadRequest)
		io.WriteString(w, err.Error())
		return
	}

	if input.Name == "" {
		input.Name = "world"
	}
	output := HelloOutput{
		Message: fmt.Sprintf("hello %s", input.Name),
	}

	w.Header().Set("Content-Type", "/application/json")
	encoder := json.NewEncoder(w)
	if err := encoder.Encode(&output); err != nil {
		panic(err)
	}
}
