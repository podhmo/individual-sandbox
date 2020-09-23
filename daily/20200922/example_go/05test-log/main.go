package main

import (
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/render"
	"golang.org/x/xerrors"
)

func main() {
	s := &Server{
		Router: chi.NewRouter(),
		logger: log.New(os.Stdout, "app", 0),
	}
	s.Mount()

	addr := os.Getenv("Addr")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, s); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type logger interface {
	Printf(string, ...interface{})
}

type Server struct {
	logger logger
	chi.Router
}

func (s *Server) Mount() {
	s.Get("/500", func(w http.ResponseWriter, r *http.Request) {
		err := f()
		if err != nil {
			s.logger.Printf("! %+v", err)
			render.Status(r, 500)
			render.JSON(w, r, map[string]string{"error": fmt.Sprintf("%v", err)})
			return
		}
		render.JSON(w, r, map[string]string{"message": "never"})
	})
}

func f() error {
	err := g()
	return xerrors.Errorf("f %w", err)
}
func g() error {
	err := h()
	return xerrors.Errorf("g %w", err)
}
func h() error {
	return fmt.Errorf("h")
}
