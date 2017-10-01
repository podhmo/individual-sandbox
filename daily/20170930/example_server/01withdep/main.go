package main

import (
	"fmt"
	"net/http"
)

// Site :
type Site struct {
	Name string
}

// SiteHandlerFunc :
type SiteHandlerFunc func(s *Site, w http.ResponseWriter, r *http.Request)

// SiteHandlerLiftFunc :
type SiteHandlerLiftFunc func(SiteHandlerFunc) http.HandlerFunc

// App :
type App struct {
	Lift SiteHandlerLiftFunc
	*http.ServeMux
}

// NewApp :
func NewApp(mux *http.ServeMux, liftfunc SiteHandlerLiftFunc) *App {
	return &App{
		ServeMux: mux,
		Lift:     liftfunc,
	}
}

// Register :
func (app *App) Register(pattern string, handler SiteHandlerFunc) {
	app.ServeMux.HandleFunc(pattern, app.Lift(handler))
}

func helloHandler(s *Site, w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello(from %s): %s", s.Name, r.URL.Path[1:])
}

func main() {
	s := &Site{
		Name: "foo",
	}
	app := NewApp(http.NewServeMux(), func(h SiteHandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			h(s, w, r)
		}
	})
	app.Register("/", helloHandler)
	http.ListenAndServe(":8080", app)
}
