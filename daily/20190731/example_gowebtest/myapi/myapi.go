package myapi

import (
	"fmt"
	"net/http"
)

// Error :
func Error(w http.ResponseWriter, error string, code int) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.Header().Set("X-Content-Type-Options", "nosniff")
	w.WriteHeader(code)
	fmt.Fprintf(w, `{"message": "%q"}`, error)
}

// Handler :
func Handler() http.Handler {
	mux := &http.ServeMux{}
	mux.HandleFunc("/", OK)
	return mux
}

func Run(port string, mux http.Handler) error {
	return http.ListenAndServe(port, mux)
}
