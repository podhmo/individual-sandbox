package main

import (
	"fmt"
	"io"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func main() {
	r := chi.NewRouter()

	r.Use(middleware.RequestID)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)
	r.Use(render.SetContentType(render.ContentTypeJSON))

	r.Get("/", func(w http.ResponseWriter, r *http.Request) {
		var p struct {
			Name string
		}
		p.Name = "GET"
		render.JSON(w, r, &p)
	})

	r.Post("/", func(w http.ResponseWriter, r *http.Request) {
		io.Copy(os.Stdout, r.Body)

		var p struct {
			Name string
		}
		p.Name = "POST"
		render.JSON(w, r, &p)
	})

	r.Get("/hello/{name}", func(w http.ResponseWriter, r *http.Request) {
		var p struct {
			Message string
		}

		name := chi.URLParam(r, "name")
		p.Message = fmt.Sprintf("Hello %s", name)
		render.JSON(w, r, &p)
	})

	// not found
	r.NotFound(func(w http.ResponseWriter, r *http.Request) {
		render.Status(r, http.StatusNotFound)
		var p struct {
			Message string
		}
		p.Message = "not-found"
		render.JSON(w, r, &p)
	})

	http.ListenAndServe(":3333", r)
}
