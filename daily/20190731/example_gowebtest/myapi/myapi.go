package myapi

import (
	"net/http"
	"time"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
)

// Handler :
func Handler() http.Handler {
	r := chi.NewRouter()
	BindHandlers(r)
	return r
}

// BindHandlers :
func BindHandlers(r chi.Router) {
	r.Get("/{status}", Status)
}

// Router :
func Router() chi.Router {
	r := chi.NewRouter()
	BindMiddlewares(r)
	BindHandlers(r)
	return r
}

// BindMiddlewares :
func BindMiddlewares(r chi.Router) {
	// A good base middleware stack
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer) // todo: json response?

	// Set a timeout value on the request context (ctx), that will signal
	// through ctx.Done() that the request has timed out and further
	// processing should be stopped.
	r.Use(middleware.Timeout(60 * time.Second))
}

func Run(port string, mux http.Handler) error {
	return http.ListenAndServe(port, mux)
}
