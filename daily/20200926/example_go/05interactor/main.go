package main

import (
	"context"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

type Todo struct {
	Title string
	Done  bool
}

type Router interface {
	Get(path string, interactor interface{}, handler http.HandlerFunc)
}

func Mount(
	r Router,
) {
	{
		ir := &TodoInteractor{}
		r.Get("/api/todo", ir.List, func(w http.ResponseWriter, r *http.Request) {
			var input TodoListInput // xxx

			items, err := ir.List(r.Context(), input)
			if err != nil {
				render.Status(r, 500)
				render.JSON(w, r, map[string]interface{}{"message": err.Error()})
				return
			}
			if items == nil {
				items = []Todo{}
			}
			render.JSON(w, r, map[string]interface{}{"items": items})
		})
	}
}

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)

	Mount(r, ip)

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type TodoInteractor struct {
}

// TodoListInput ...
type TodoListInput struct {
	// OrderBy ...
	// @type=query
	// @required=false
	OrderBy string
}

func (ir *TodoInteractor) List(context.Context, TodoListInput) ([]Todo, error) {
	return nil, nil
}
func (ir *TodoInteractor) Resolve(ctx context.Context, todoID string) (Todo, error) {
	return Todo{}, nil
}
