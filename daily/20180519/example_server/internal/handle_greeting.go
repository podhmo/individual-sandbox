package internal

import (
	"encoding/json"
	"fmt"
	"net/http"
)

func (s *server) handleGreeting(format string) http.HandlerFunc {
	type request struct {
		Name string `json:"name"`
	}
	type response struct {
		Message string `json:"message"`
	}

	return func(w http.ResponseWriter, r *http.Request) {
		var req request
		decoder := json.NewDecoder(r.Body)
		defer r.Body.Close()
		if err := decoder.Decode(&req); err != nil {
			BadRequest(w, r)
			return
		}
		encoder := json.NewEncoder(w)
		encoder.SetIndent("", "  ")
		v := response{
			Message: fmt.Sprintf(format, req.Name),
		}
		if err := encoder.Encode(&v); err != nil {
			InternalServerError(w, r)
		}
	}
}
