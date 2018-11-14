package internal

import (
	"fmt"
	"net/http"
)

// Server :
type Server struct {
	mux *http.ServeMux
}

// NewServer :
func NewServer() *Server {
	return &Server{mux: &http.ServeMux{}}
}

// Handler :
func (s *Server) Handler() http.Handler {
	s.mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Add("Content-Type", "application/json")
		fmt.Fprintf(w, `{"url": "%s"}`, r.URL.Path)
	})
	return s.mux
}
