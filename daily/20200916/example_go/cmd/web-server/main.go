package main

import (
	"encoding/json"
	"log"
	"m/store"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func NewServer() http.Handler {
	r := chi.NewRouter()
	s := store.NewTodoStore()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	// r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	r.Get("/api/todos", func(w http.ResponseWriter, r *http.Request) {
		items := s.List()
		if items == nil {
			items = []store.Todo{}
		}
		// TODO: https://opensource.zalando.com/restful-api-guidelines
		render.JSON(w, r, map[string]interface{}{"items": items})
	})
	r.Post("/api/todos", func(w http.ResponseWriter, r *http.Request) {
		decoder := json.NewDecoder(r.Body)
		var item store.Todo
		if err := decoder.Decode(&item); err != nil {
			render.Status(r, 400)
			render.JSON(w, r, err)
			return
		}
		defer r.Body.Close()
		s.Add(item)
		w.WriteHeader(http.StatusCreated)
		render.JSON(w, r, &item)
	})
	return r
}

func run() error {
	port := os.Getenv("PORT")
	if port == "" {
		port = ":50051"
	}
	log.Println("listening ...", port)

	r := NewServer()
	return http.ListenAndServe(port, r)
}
