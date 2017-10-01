package main

import (
	"fmt"
	"net/http"

	"encoding/json"

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
	DB *DB
}

// DB :
type DB struct {
}

// GetUser :
func (db *DB) GetUser(id string) *User {
	return &User{
		Name: "foo",
		Age:  20,
	}
}

// User :
type User struct {
	Name string `json:"name"`
	Age  uint   `json:"age"`
}

// AppResponse :
type AppResponse interface {
	Status() int
	Write(w http.ResponseWriter) error
}

// FormatResponse :
type FormatResponse struct {
	status int
	fmt    string
	args   []interface{}
}

// Status :
func (r *FormatResponse) Status() int {
	return r.status
}

// Write :
func (r *FormatResponse) Write(w http.ResponseWriter) error {
	w.WriteHeader(r.Status())
	fmt.Fprintf(w, r.fmt, r.args...) // todo zero copy ?
	return nil
}

// JSONResponse :
type JSONResponse struct {
	ob     interface{}
	status int
}

// Status :
func (r *JSONResponse) Status() int {
	return r.status
}

// Write :
func (r *JSONResponse) Write(w http.ResponseWriter) error {
	w.WriteHeader(r.Status())
	encoder := json.NewEncoder(w)
	encoder.SetIndent("", "  ")
	return encoder.Encode(r.ob)
}

// AppHandlerFunc :
type AppHandlerFunc func(registry *Registry, r *http.Request) (AppResponse, error)

// AppHandlerLiftFunc :
type AppHandlerLiftFunc func(AppHandlerFunc) http.HandlerFunc

// newAppRegistry :
func newAppRegistry() *Registry {
	return &Registry{
		DB: &DB{},
	}
}

func main() {
	registry := newAppRegistry()

	router := httptreemux.NewContextMux()
	group := router.NewGroup("/api")

	lift := func(h AppHandlerFunc) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			res, err := h(registry, r)
			w.Header().Add("Content-Type", "application/json")
			if err == nil {
				if err := res.Write(w); err == nil {
					return
				}
			}

			if err, ok := err.(AppError); ok {
				w.WriteHeader(err.Status())
				fmt.Fprintln(w, err.Message())
			} else {
				w.WriteHeader(http.StatusInternalServerError)
				fmt.Fprintf(w, "%+v", err)
			}
		}
	}

	group.GET("/:id", lift(helloHandler))
	http.ListenAndServe(":8080", router)
}

func authCheck(r *http.Request, id string) error {
	if id == "kill" {
		return Forbidden("you want to be suspended?")
	}
	return nil
}

func helloHandler(registry *Registry, r *http.Request) (AppResponse, error) {
	params := httptreemux.ContextParams(r.Context())
	id := params["id"]

	if err := authCheck(r, id); err != nil {
		return nil, err
	}

	user := registry.DB.GetUser(id)

	// todo: template response
	// return &FormatResponse{
	// 	status: 200,
	// 	fmt:    "hello(from %s): %s",
	// 	args:   []interface{}{user.Name, id},
	// }, nil
	return &JSONResponse{
		status: 200,
		ob:     user,
	}, nil
}
