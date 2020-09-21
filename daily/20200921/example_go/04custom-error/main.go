package main

import (
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
	"golang.org/x/xerrors"
)

type CustomHandler func(w http.ResponseWriter, r *http.Request) error

type Server struct {
	r              chi.Router
	defaultHandler func(CustomHandler) http.HandlerFunc
}

func (s *Server) Get(path string, h CustomHandler) {
	s.r.Get(path, s.defaultHandler(h))
}
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	s.r.ServeHTTP(w, r)
}

func defaultHandler(ch CustomHandler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		err := ch(w, r)
		if err == nil {
			return
		}
		// todo: panic recovery?
		// todo: 400などのerrorのときにはそれ用のhandling

		render.Status(r, 500)
		log.Printf("! %+v", err) // todo: まともなlog
		render.JSON(w, r, map[string]string{"error": err.Error()}) // Unwrap()したほうが良いかも？
	}
}
func main() {
	s := &Server{
		r:              chi.NewRouter(),
		defaultHandler: defaultHandler,
	}
	s.r.Use(middleware.Logger)

	s.Get("/", func(w http.ResponseWriter, r *http.Request) error {
		return xerrors.Errorf("/ %w", fmt.Errorf("ng"))
	})

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":8888"
	}

	log.Printf("listen %v", addr)
	if err := http.ListenAndServe(addr, s); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
