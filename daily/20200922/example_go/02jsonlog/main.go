package main

import (
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/httplog"
	"github.com/go-chi/render"
)

func main() {
	// これと
	l := httplog.NewLogger("app", httplog.Options{
		JSON: true,
	})

	r := chi.NewRouter()
	// r.Use(middlware.Logger) // 消す
	r.Use(httplog.RequestLogger(l)) // これ

	r.Get("/api", func(w http.ResponseWriter, r *http.Request) {
		data := map[string]string{
			"message": "hello",
		}
		render.JSON(w, r, data)
	})

	addr := os.Getenv("Addr")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
