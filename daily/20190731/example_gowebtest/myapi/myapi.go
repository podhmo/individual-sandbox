package myapi

import (
	"net/http"

	"github.com/go-chi/chi"
)

// Handler :
func Handler() http.Handler {
	r := chi.NewRouter()
	r.Get("/{status}", Status)
	return r
}

func Run(port string, mux http.Handler) error {
	return http.ListenAndServe(port, mux)
}
