package main

import (
	"fmt"
	"net/http"
)

// Registry :
type Registry struct {
	Site *Site
}

// Site :
type Site struct {
	Name string
}

// AppHandlerFunc :
type AppHandlerFunc func(registry *Registry, w http.ResponseWriter, r *http.Request)

// AppHandlerLiftFunc :
type AppHandlerLiftFunc func(AppHandlerFunc) http.HandlerFunc

// App :
type App struct {
	Lift AppHandlerLiftFunc
	*http.ServeMux
}

// NewApp :
func NewApp(mux *http.ServeMux, liftfunc AppHandlerLiftFunc) *App {
	return &App{
		ServeMux: mux,
		Lift:     liftfunc,
	}
}

// Register :
func (app *App) Register(pattern string, handler AppHandlerFunc) {
	app.ServeMux.HandleFunc(pattern, app.Lift(handler))
}

func helloHandler(registry *Registry, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello(from %s): %s", registry.Site.Name, r.URL.Path[1:])
}

// newAppRegistry :
func newAppRegistry() *Registry {
	return &Registry{
		Site: &Site{
			Name: "foo",
		},
	}
}

func main() {
	registry := newAppRegistry()

	app := NewApp(http.NewServeMux(), func(h AppHandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			h(registry, w, r)
		}
	})
	app.Register("/", helloHandler)
	http.ListenAndServe(":8080", app)
}
