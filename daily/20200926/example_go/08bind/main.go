package main

import (
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func init() {
	render.Decode = func(r *http.Request, v interface{}) error {
		return render.DecodeJSON(r.Body, v)
	}
}

type Input struct {
	BoardID string `json:"boardID"`
	Todo    Todo
}

func (i *Input) Bind(r *http.Request) error {
	i.BoardID = chi.RouteContext(r.Context()).URLParam("boardID")
	return render.DecodeJSON(r.Body, &i.Todo)
}

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Post("/api/board/{boardID}/todo", func(w http.ResponseWriter, r *http.Request) {
		var ob Input
		if err := render.DecodeJSON(r.Body, &ob.Todo); err != nil {
			render.Status(r, 400)
			render.JSON(w, r, map[string]string{"error": err.Error()})
			return
		}
		ob.BoardID = chi.RouteContext(r.Context()).URLParam("boardID")
		render.JSON(w, r, ob)
	})

	r.Post("/api/board/{boardID}/todo2", func(w http.ResponseWriter, r *http.Request) {
		var ob Input
		if err := render.Bind(r, &ob); err != nil {
			render.Status(r, 400)
			render.JSON(w, r, map[string]string{"error": err.Error()})
			return
		}
		render.JSON(w, r, ob)
	})

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
