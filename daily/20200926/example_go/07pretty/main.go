package main

import (
	"encoding/json"
	"log"
	"net/http"
	"os"
	"reflect"
	"strconv"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func JSON(w http.ResponseWriter, r *http.Request, v interface{}) {
	// Force to return empty JSON array [] instead of null in case of zero slice.
	val := reflect.ValueOf(v)
	if val.Kind() == reflect.Slice && val.IsNil() {
		v = reflect.MakeSlice(val.Type(), 0, 0).Interface()
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	if status, ok := r.Context().Value(render.StatusCtxKey).(int); ok {
		w.WriteHeader(status)
	}

	encoder := json.NewEncoder(w)
	if ok, _ := strconv.ParseBool(r.URL.Query().Get("pretty")); ok {
		encoder.SetIndent("", "  ")
	}
	if err := encoder.Encode(v); err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)

	r.Get("/api/xxx", func(w http.ResponseWriter, r *http.Request) {
		var data []interface{}
		render.JSON(w, r, data)
	})
	r.Get("/api/yyy", func(w http.ResponseWriter, r *http.Request) {
		var v []interface{}
		JSON(w, r, v)
	})
	r.Get("/api/xxx2", func(w http.ResponseWriter, r *http.Request) {
		v := []interface{}{1, map[string]string{"foo": "bar"}, 3, "foo", 4, 5}
		render.JSON(w, r, v)
	})
	r.Get("/api/yyy2", func(w http.ResponseWriter, r *http.Request) {
		v := []interface{}{1, map[string]string{"foo": "bar"}, 3, "foo", 4, 5}
		JSON(w, r, v)
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
