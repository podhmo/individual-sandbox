package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Get("/api", func(w http.ResponseWriter, r *http.Request) {
		data := map[string]string{
			"message": "hello",
		}
		render.JSON(w, r, data)
	})

	r.Post("/api/hello", func(w http.ResponseWriter, r *http.Request) {
		// {"target": "someone"}
		params := map[string]string{}
		decoder := json.NewDecoder(r.Body)
		if err := decoder.Decode(&params); err != nil {
			render.Status(r, 400)
			render.JSON(w, r, map[string]interface{}{
				"error": err.Error(),
			})
		}

		defer r.Body.Close()
		data := map[string]string{
			"message": fmt.Sprintf("hello %s", params["target"]),
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
