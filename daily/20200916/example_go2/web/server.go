package web

import (
	"encoding/json"
	"fmt"
	"m/store"
	"net/http"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func NewServer() http.Handler {
	r := chi.NewRouter()
	s := store.NewTodoStore()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	// r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	r.Get("/api/todos", func(w http.ResponseWriter, r *http.Request) {
		var items []*store.Todo
		if err := s.List(r.Context(), &items); err != nil {
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{
				"message": err,
			})
			return
		}
		if items == nil {
			items = []*store.Todo{}
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
		if err := s.Add(r.Context(), &item); err != nil {
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{
				"message": err,
			})
			return
		}
		render.Status(r, 201)
		render.JSON(w, r, &item)
	})

	// default 404 handler
	r.NotFound(func(w http.ResponseWriter, r *http.Request) {
		render.Status(r, 404)
		render.JSON(w, r, map[string]interface{}{
			"method":  r.Method,
			"path":    r.URL.RequestURI(),
			"query":   r.URL.RawQuery,
			"code":    "not found",
			"message": fmt.Sprintf("%s %s is not found", r.Method, r.URL.RequestURI()),
		})
	})

	// TODO unauthorized
	return r
}
