package internal

import (
	"encoding/json"
	"net/http"
)

func (s *server) handleAbout() http.HandlerFunc {
	type response struct {
		Version string `json:"version"`
	}

	return func(w http.ResponseWriter, r *http.Request) {
		encoder := json.NewEncoder(w)
		encoder.SetIndent("", "  ")
		v := response{
			Version: s.Version,
		}
		if err := encoder.Encode(&v); err != nil {
			InternalServerError(w, r)
		}
	}
}

