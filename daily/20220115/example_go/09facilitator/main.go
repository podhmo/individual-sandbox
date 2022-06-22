package main

import "net/http"
import "encoding/json"

type Router interface {
	Get(string, http.HandlerFunc)
}

// Routing is the facilitator for registering endpoint
// see: https://rakyll.org/generics-facilititators/
type Routing[T any] struct {
	router Router
}

func (r *Routing[T]) Get(path string, action func(http.ResponseWriter, *http.Request) (T, error)) {
	r.router.Get(path, lift(action))
}

func lift[T any](action func(http.ResponseWriter, *http.Request) (T, error)) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		result, err := action(w, req)
		if err != nil {
			http.Error(w, err.Error(), 500)
			return
		}
		w.Header().Set("Content-Type", "encoding/json")
		_ = json.NewEncoder(w).Encode(result) // TODO: error handling
	}
}

func route[T any](r Router) *Routing[T] {
	return &Routing[T]{router: r}
}

func mount(r Router){
	route[int64](r).Get("/int", func(http.ResponseWriter, *http.Request) (int64, error) { return 10, nil} )
	route[string](r).Get("/string", func(http.ResponseWriter, *http.Request) (string, error) { return "foo", nil} )
}

func main(){
}
