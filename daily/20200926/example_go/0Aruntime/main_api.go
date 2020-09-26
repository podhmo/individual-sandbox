// +build api

package main

import (
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

type mounter struct {
}

func (m *mounter) MountHello(r Router, ir *HelloInteractor) {
	r.Get("/api", func(w http.ResponseWriter, r *http.Request) {
		data := map[string]string{
			"message": ir.Hello(),
		}
		render.JSON(w, r, data)
	})
}

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)
	Mount(&mounter{}, r)

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
