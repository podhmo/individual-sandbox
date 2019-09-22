package main

import (
	"fmt"
	"net/http"
	"time"

	rfc3339 "github.com/podhmo/go-rfc3339"
)

type Server struct {
	Now func() time.Time
}

func (s *Server) now(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, `{"now": "%s"}`, rfc3339.Format(s.Now()))
}

func (s *Server) Handler() http.Handler {
	var mux http.ServeMux
	mux.HandleFunc("/now", s.now)
	return &mux
}

func main() {
	s := &Server{Now: time.Now}
	http.ListenAndServe(":8080", s.Handler())
}
