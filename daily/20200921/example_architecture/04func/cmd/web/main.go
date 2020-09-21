package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"m/04func/internal"
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
	r := &internal.Resource{
		Store:  store,
		Output: os.Stdout,
		Debug:  os.Stderr,
	}
	defer func() {
		// xxx: for -save
		store = r.Store
	}()

	router := chi.NewRouter()
	{
		router.Use(middleware.RequestID)
		router.Use(middleware.Logger)
		router.Use(middleware.Recoverer)

		router.Get("/api/todos", func() http.HandlerFunc {
			interact := internal.NewList(r)
			return func(w http.ResponseWriter, req *http.Request) {
				items, err := interact()
				if err != nil {
					render.Status(req, 500)
					render.JSON(w, req, map[string]interface{}{"message": err.Error()})
				}
				if items == nil {
					items = []internal.Todo{}
				}
				render.JSON(w, req, map[string]interface{}{"items": items})
			}
		}())

		router.Post("/api/todos", func() http.HandlerFunc {
			interact := internal.NewAdd(r)
			return func(w http.ResponseWriter, req *http.Request) {
				var item internal.Todo
				decoder := json.NewDecoder(req.Body)
				err := decoder.Decode(&item)
				if err != nil {
					render.Status(req, 400)
					render.JSON(w, req, map[string]interface{}{"message": err.Error()})
				}
				defer req.Body.Close()

				items, err := interact(item)
				if err != nil {
					render.Status(req, 500)
					render.JSON(w, req, map[string]interface{}{"message": err.Error()})
				}
				render.JSON(w, req, map[string]interface{}{"items": items})
			}
		}())

		router.Patch("/api/todos/{n}", func() http.HandlerFunc {
			interact := internal.NewDone(r)
			return func(w http.ResponseWriter, req *http.Request) {
				nStr := chi.URLParam(req, "n")
				n, err := strconv.ParseInt(nStr, 10, 64)
				if err != nil {
					render.Status(req, 400)
					render.JSON(w, req, map[string]interface{}{"message": err.Error()})
				}
				items, err := interact(int(n))
				if err != nil {
					render.Status(req, 500)
					render.JSON(w, req, map[string]interface{}{"message": err.Error()})
				}
				render.JSON(w, req, map[string]interface{}{"items": items})
			}
		}())
	}
	return http.ListenAndServe(fmt.Sprintf(":%d", port), router)
}
