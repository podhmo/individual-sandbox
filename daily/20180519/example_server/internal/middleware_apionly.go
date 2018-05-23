package internal

import (
	"net/http"
	"strings"
)

func (s *server) APIOnly(h http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if strings.ToLower(r.Header.Get("Content-Type")) != "/application/json" {
			http.NotFound(w, r)
		}
		h(w, r)
	}
}
