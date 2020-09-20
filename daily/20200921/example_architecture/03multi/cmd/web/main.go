package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"m/03multi/internal"
	"net/http"
	"os"
	"strconv"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

var (
	debug = false
	store []internal.Todo
)

func init() {
	store = []internal.Todo{
		{Title: "Go to bed", Done: false},
	}
}

func main() {
	filenameFlag := flag.String("store", "", "store file name (e.g. store.json)")
	saveFlag := flag.Bool("save", false, "save")
	portFlag := flag.Int("port", 8888, "port")
	flag.Parse()

	loader := &internal.Loader{}
	if filenameFlag != nil && *filenameFlag != "" {
		if err := loader.Load(*filenameFlag, &store); err != nil {
			log.Printf("! %+v", err)
			os.Exit(1)
		}
	}

	if err := run(*portFlag); err != nil {
		log.Fatalf("!! %+v", err)
	}

	if saveFlag != nil && *saveFlag {
		if filenameFlag != nil && *filenameFlag != "" {
			if err := loader.Save(*filenameFlag, store); err != nil {
				log.Printf("! %+v", err)
				os.Exit(1)
			}
		}
	}
}

func run(
	port int,
) error {
	ir := &internal.Interactor{
		Debug:  debug,
		Writer: os.Stderr,
		Store:  store,
	}

	r := chi.NewRouter()
	{
		r.Use(middleware.RequestID)
		r.Use(middleware.Logger)
		r.Use(middleware.Recoverer)

		r.Get("/api/todos", func(w http.ResponseWriter, r *http.Request) {
			items, err := ir.List()
			if err != nil {
				render.Status(r, 500)
				render.JSON(w, r, map[string]interface{}{"message": err.Error()})
			}
			if items == nil {
				items = []internal.Todo{}
			}
			render.JSON(w, r, map[string]interface{}{"items": items})
		})

		r.Post("/api/todos", func(w http.ResponseWriter, r *http.Request) {
			var item internal.Todo
			decoder := json.NewDecoder(r.Body)
			err := decoder.Decode(&item)
			if err != nil {
				render.Status(r, 400)
				render.JSON(w, r, map[string]interface{}{"message": err.Error()})
			}
			defer r.Body.Close()

			items, err := ir.Add(item)
			if err != nil {
				render.Status(r, 500)
				render.JSON(w, r, map[string]interface{}{"message": err.Error()})
			}
			render.JSON(w, r, map[string]interface{}{"items": items})
		})

		r.Patch("/api/todos/{n}", func(w http.ResponseWriter, r *http.Request) {
			nStr := chi.URLParam(r, "n")
			n, err := strconv.ParseInt(nStr, 10, 64)
			if err != nil {
				render.Status(r, 400)
				render.JSON(w, r, map[string]interface{}{"message": err.Error()})
			}
			items, err := ir.Done(int(n))
			if err != nil {
				render.Status(r, 500)
				render.JSON(w, r, map[string]interface{}{"message": err.Error()})
			}
			render.JSON(w, r, map[string]interface{}{"items": items})
		})
	}

	// xxx: for -save
	defer func() {
		store = ir.Store
	}()
	return http.ListenAndServe(fmt.Sprintf(":%d", port), r)
}
