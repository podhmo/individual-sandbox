package main

import (
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func NewHandler() http.Handler {
	r := chi.NewRouter()
	r.Use(middleware.BasicAuth("realm?", map[string]string{
		"user1": "password1",
	}))

	r.Get("/hello", func(w http.ResponseWriter, r *http.Request) {
		render.JSON(w, r, map[string]string{"message": "hello"})
	})
	return r
}

func main() {
	r := NewHandler()

	addr := os.Getenv("Addr")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
