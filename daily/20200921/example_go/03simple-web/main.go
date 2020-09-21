package main

import (
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/render"
	"github.com/go-chi/chi/middleware"
)

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)
	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		render.JSON(w, r, map[string]string{"message": "hello"})
	})
	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":8888"
	}
	log.Println("listen:", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("%+v", err)
	}
}
