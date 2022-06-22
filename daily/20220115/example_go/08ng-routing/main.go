package main

import "net/http"
import "encoding/json"

type Router interface {
	Get(string, http.HandlerFunc)
}

var router Router // TODO: 通常は初期化されている

func Get(path string, action func(http.ResponseWriter, *http.Request) (any, error)) {
	router.Get(path, lift(action))
}

func lift(action func(http.ResponseWriter, *http.Request) (any, error)) http.HandlerFunc {
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
	Get("/ints", func(http.ResponseWriter, *http.Request) (int64, error) { return 10, nil} )
	Get("/strings", func(http.ResponseWriter, *http.Request) (string, error) { return "foo", nil} )
}

func main(){
}
