package internal

import (
	"net/http"
)

type server struct {
	router  *http.ServeMux
	Version string
}

func newServer() *server {
	s := &server{
		router:  &http.ServeMux{},
		Version: "1.0",
	}
	s.routes()
	return s
}

func (s *server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.router.ServeHTTP(w, r)
}
