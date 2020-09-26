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

type TodoInteractor struct {
}

func (ir *TodoInteractor) List(context.Context) ([]Todo, error) {
	return nil, nil
}

type Handler interface {
	Handler() http.HandlerFunc
	Interactor() interface{}
}

type Router interface {
	Get(string, Handler)
}

type handler struct {
	interact interface{}
	handle   func(w http.ResponseWriter, r *http.Request)
}

func (h *handler) Handler() http.HandlerFunc {
	return h.handle
}
func (h *handler) Interactor() interface{} {
	return h.interact
}
func NewHandler(h http.HandlerFunc, ir interface{}) Handler {
	return &handler{handle: h, interact: ir}
}
func Mount(r Router) {
	{
		ir := &TodoInteractor{}
		r.Get("/api/todo", NewHandler(
			func(w http.ResponseWriter, r *http.Request) {
				items, err := ir.List(r.Context())
				if err != nil {
					render.Status(r, 500)
					render.JSON(w, r, map[string]interface{}{"message": err.Error()})
				}
				if items == nil {
					items = []Todo{}
				}
				render.JSON(w, r, map[string]interface{}{"items": items})
			},
			ir.List,
		))
	}
}

type FakeRouter struct{}

func (r *FakeRouter) Get(path string, h Handler) {
	// func() error
	// func(context.Context) error
	// func(context.Context, in interface{}) error
	// func() (interface{}, error)
	// func(context.Context) (interface{}, error)
	// func(context.Context, in interface{}) (interface{}, error)

	fmt.Printf("mount GET %s, %T\n", path, h.Interactor())
}

type APIRouter struct {
	chi.Router
}

func (r *APIRouter) Get(path string, h Handler) {
	r.Router.Get(path, http.HandlerFunc(h.Handler()))
}
func main() {
	{
		r := &FakeRouter{}
		Mount(r)
	}

	{
		r := chi.NewRouter()
		r.Use(middleware.Logger)

		rr := &APIRouter{Router: r}
		Mount(rr)
		addr := os.Getenv("ADDR")
		if addr == "" {
			addr = ":4444"
		}

		log.Printf("listen: %s", addr)
		if err := http.ListenAndServe(addr, r); err != nil {
			log.Fatalf("!! %+v", err)
		}
	}
}
