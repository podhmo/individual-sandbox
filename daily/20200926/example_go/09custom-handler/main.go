package main

import (
	"encoding/json"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

func DecodeJSON(r io.Reader, v interface{}) error {
	defer io.Copy(ioutil.Discard, r)
	return json.NewDecoder(r).Decode(v)
}

func init() {
	render.Decode = func(r *http.Request, v interface{}) error {
		return DecodeJSON(r.Body, v)
	}
}

type HasStatusCode interface {
	StatusCode() int
}

type ApplicationError struct {
	Err  error
	Code int
}

func (e ApplicationError) Unwrap() error {
	return e.Err
}

func (e ApplicationError) Error() string {
	return e.Err.Error()
}

func (e ApplicationError) StatusCode() int {
	return e.Code
}

type Input struct {
	BoardID string `json:"boardID"`
	Todo    Todo
}

func (i *Input) Bind(r *http.Request) error {
	i.BoardID = chi.RouteContext(r.Context()).URLParam("boardID")
	err := DecodeJSON(r.Body, &i.Todo)
	if err != nil {
		return ApplicationError{Err: err, Code: 400}
	}
	return nil
}

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

func main() {
	r := chi.NewRouter()
	r.Use(middleware.Logger)

	lift := func(h func(w http.ResponseWriter, r *http.Request) error) http.HandlerFunc {
		return func(w http.ResponseWriter, r *http.Request) {
			err := h(w, r)
			if err == nil {
				return
			}

			status := 500
			if inner, ok := err.(HasStatusCode); ok {
				status = inner.StatusCode()
			}
			render.Status(r, status)
			render.JSON(w, r, map[string]string{"message": err.Error()})
		}
	}

	r.Post("/api/board/{boardID}/todo", lift(func(w http.ResponseWriter, r *http.Request) error {
		var ob Input
		if err := (&ob).Bind(r); err != nil {
			return err
		}
		render.JSON(w, r, ob)
		return nil
	}))

	addr := os.Getenv("ADDR")
	if addr == "" {
		addr = ":4444"
	}

	log.Printf("listen: %s", addr)
	if err := http.ListenAndServe(addr, r); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
