package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
	"github.com/podhmo/startevent"
)

func main() {
	addr := os.Getenv("Addr")
	if addr == "" {
		addr = ":4444"
	}
	port, err := strconv.Atoi(addr[1:])
	if err != nil {
		// 多くなってきたらmain()とrun()に分けるべきかも
		log.Fatalf("!%+v", err)
	}

	if os.Getenv("SENTINEL") != "" {
		startevent.Config{
			URL: fmt.Sprintf("http://localhost:%d/ping", port),
		}.Run(context.Background(), os.Getenv("SENTINEL"))
	}

	r := chi.NewRouter()
	r.Use(middleware.Logger)
	r.Use(middleware.Heartbeat("/ping"))

	r.Get("/api", func(w http.ResponseWriter, r *http.Request) {
		data := map[string]string{
			"message": "hello",
		}
		render.JSON(w, r, data)
	})

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
