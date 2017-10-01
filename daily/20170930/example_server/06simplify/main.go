package main

import (
	"encoding/json"
	"fmt"
	"net/http"

	"github.com/dimfeld/httptreemux"
)

// Registry :
type Registry struct {
	DB *DB
}

// DB :
type DB struct {
}

// GetUser :
func (db *DB) GetUser(id string) *User {
	return &User{
		Name: "foo",
		Age:  20,
	}
}

// User :
type User struct {
	Name string `json:"name"`
	Age  uint   `json:"age"`
}

// AppHandlerFunc :
type AppHandlerFunc func(registry *Registry, w http.ResponseWriter, r *http.Request)

// newAppRegistry :
func newAppRegistry() *Registry {
	return &Registry{
		DB: &DB{},
	}
}

func main() {
	registry := newAppRegistry()

	router := httptreemux.NewContextMux()
	group := router.NewGroup("/api")

	lift := func(h AppHandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			h(registry, w, r)
		}
	}

	group.GET("/:id", lift(helloHandler))
	http.ListenAndServe(":8080", router)
}

func helloHandler(registry *Registry, w http.ResponseWriter, r *http.Request) {
	params := httptreemux.ContextParams(r.Context())
	id := params["id"]

	if id == "kill" {
		w.WriteHeader(http.StatusForbidden)
		w.Write([]byte("you want to be suspended?"))
		return
	}

	user := registry.DB.GetUser(id)
	encoder := json.NewEncoder(w)
	encoder.SetIndent("", "  ")
	if err := encoder.Encode(user); err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprintf(w, "%+v\n", err)
		return
	}
	return
}
