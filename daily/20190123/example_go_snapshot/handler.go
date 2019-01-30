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
	b, err := json.Marshal(&output)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		encoder := json.NewEncoder(w)
		if err := encoder.Encode(map[string]string{"error": err.Error()}); err != nil {
			panic(err)
		}
		return
	}
	w.Write(b)
}
