package main

import (
	"fmt"
	"net/http"
)

// AppError :
type AppError interface {
	Status() int
	Message() string
}

// ConstantAppError :
type ConstantAppError struct {
	status  int
	message string
	raw     error
}

// Status :
func (e *ConstantAppError) Status() int {
	return e.status
}

// Message :
func (e *ConstantAppError) Message() string {
	return e.message
}

// Error :
func (e *ConstantAppError) Error() string {
	if e.raw == nil {
		return e.message
	}
	return e.raw.Error()
}

// Forbidden :
func Forbidden(message string) error {
	return &ConstantAppError{
		status:  http.StatusForbidden,
		message: message,
	}
}

// InternalServerError :
func InternalServerError(err error, message string) error {
	return &ConstantAppError{
		status:  http.StatusInternalServerError,
		message: message,
		raw:     err,
	}
}

// Registry :
type Registry struct {
	Site *Site
}

// Site :
type Site struct {
	Name string
}

// AppHandlerFunc :
type AppHandlerFunc func(registry *Registry, w http.ResponseWriter, r *http.Request) error

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

func helloHandler(registry *Registry, w http.ResponseWriter, r *http.Request) error {
	if r.URL.Path[1:] == "kill" {
		return Forbidden("you want to be suspended?")
	}

	fmt.Fprintf(w, "hello(from %s): %s", registry.Site.Name, r.URL.Path[1:])
	return nil
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
			err := h(registry, w, r)
			if err == nil {
				return
			}

			if err, ok := err.(AppError); ok {
				w.WriteHeader(err.Status())
				w.Write([]byte(err.Message()))
			} else {
				w.WriteHeader(http.StatusInternalServerError)
				fmt.Fprintf(w, "%+v", err)
			}
		}
	})

	app.Register("/", helloHandler)
	http.ListenAndServe(":8080", app)
}
