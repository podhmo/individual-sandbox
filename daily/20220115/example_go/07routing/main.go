package main

import "net/http"
import "encoding/json"

type Router interface {
	Get(string, http.HandlerFunc)
}

var router Router // TODO: 通常は初期化されている

func Get[T any](path string, action func(http.ResponseWriter, *http.Request) (T, error)) {
	router.Get(path, lift(action))
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

func mount(){
	Get("/int", func(http.ResponseWriter, *http.Request) (int64, error) { return 10, nil} )
	Get("/string", func(http.ResponseWriter, *http.Request) (string, error) { return "foo", nil} )
}

func main(){
}
