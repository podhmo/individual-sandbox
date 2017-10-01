package main

import (
	"fmt"
	"net/http"

	"github.com/dimfeld/httptreemux"
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

	router := httptreemux.NewContextMux()
	group := router.NewGroup("/api")

	lift := func(h AppHandlerFunc) http.HandlerFunc {
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
	}

	group.GET("/:id", lift(helloHandler))
	http.ListenAndServe(":8080", router)
}

func helloHandler(registry *Registry, w http.ResponseWriter, r *http.Request) error {
	if r.URL.Path[1:] == "kill" {
		return Forbidden("you want to be suspended?")
	}

	params := httptreemux.ContextParams(r.Context())
	id := params["id"]
	fmt.Fprintf(w, "hello(from %s): %s", registry.Site.Name, id)
	return nil
}
