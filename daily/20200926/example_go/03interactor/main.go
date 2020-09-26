package main

import (
	"context"
	"fmt"
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

// TodoListInput ...
type TodoListInput struct {
	// OrderBy ...
	// @type=query
	// @required=false
	OrderBy string
}
type TodoInteractor interface {
	// List list todo
	// @method=GET
	// @path=/api/todo
	List(context.Context, TodoListInput) ([]Todo, error)

	// Resolve get todo
	// @method=GET
	// @path=/api/todo/{todoID}
	Resolve(ctx context.Context, todoID string) (Todo, error)
}

type InteractorProvider struct {
	TodoInteractor func(context.Context) TodoInteractor
}

type Router interface {
	Register(method string, path string, handler http.HandlerFunc)
}

func Mount(
	r Router,
	ip *InteractorProvider,
) {
	notImplemented := func(method, path string) {
		message := fmt.Sprintf("⚠ %s %s is not implemented yet", method, path)
		log.Printf("\x1b[33m" + message + "\x1b[0m")

		// TODO: handling method
		r.Register(method, path, func(w http.ResponseWriter, r *http.Request) {
			render.Status(r, 500)
			render.JSON(w, r, map[string]interface{}{
				"message": message,
			})
		})
	}

	{
		if ip.TodoInteractor == nil {
			notImplemented("GET", "/api/todo")
		} else {
			r.Register("GET", "/api/todo", func(w http.ResponseWriter, r *http.Request) {
				var input TodoListInput // xxx

				interact := ip.TodoInteractor(r.Context()).List
				items, err := interact(r.Context(), input)
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
}

func main() {
	ip := &InteractorProvider{
		TodoInteractor: func(context.Context) TodoInteractor {
			return &todoInteractor{}
		},
	}
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

type todoInteractor struct {
}

func (ir *todoInteractor) List(context.Context, TodoListInput) ([]Todo, error) {
	return nil, nil
}
func (ir *todoInteractor) Resolve(ctx context.Context, todoID string) (Todo, error) {
	return Todo{}, nil
}
