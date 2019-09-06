package main

import (
	"fmt"
	"net/http"
	"time"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
)

func handler(w http.ResponseWriter, r *http.Request) {
	basicAuthUser := chi.URLParam(r, "user")
	basicAuthPassword := chi.URLParam(r, "pass")
	user, pass, ok := r.BasicAuth()
	if !ok || user != basicAuthUser || pass != basicAuthPassword {
		w.Header().Add("WWW-Authenticate", `Basic realm="Fake Realm"`)
		w.WriteHeader(http.StatusUnauthorized)
		fmt.Fprintf(w, `{"authenticated": false}`)
		return
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	fmt.Fprintf(w, `{"authenticated": true, "user": %q}`, user)
}

// Router :
func Router() http.Handler {
	r := chi.NewRouter()

	// A good base middleware stack
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	r.Use(middleware.Timeout(60 * time.Second))

	r.Get("/auth/basic-auth/{user}/{pass}", handler)
	return r
}

func main() {
	r := Router()
	http.ListenAndServe(":3000", r)
}
