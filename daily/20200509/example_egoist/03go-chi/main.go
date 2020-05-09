package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	"m/model" // defined by us

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	r := chi.NewRouter()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	// r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	r.Post("/api/articles", func(w http.ResponseWriter, r *http.Request) {
		decoder := json.NewDecoder(r.Body)
		var ob model.Article
		if err := decoder.Decode(&ob); err != nil {
			render.Status(r, 400)
			render.JSON(w, r, err)
			return
		}
		defer r.Body.Close()
		render.JSON(w, r, &ob)
	})

	port := os.Getenv("PORT")
	return http.ListenAndServe(fmt.Sprintf(":%s", port), r)
}
