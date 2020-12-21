package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"

	"github.com/podhmo/reflect-openapi/pkg/shape"
)

func Hello(name string) string {
	return fmt.Sprintf("Hello %s", name)
}

type Event struct {
	Body io.ReadCloser
}

type AppError struct {
	error
	Status int
}

type HelloHandler func(string) string

func (action HelloHandler) Handle(ctx context.Context, ev Event) (interface{}, error) {
	var body struct {
		Name string `json:"name"`
	}
	if err := json.NewDecoder(ev.Body).Decode(&body); err != nil {
		return nil, &AppError{Status: 400, error: err}
	}
	defer ev.Body.Close()
	return action(body.Name), nil
}

type Handler interface {
	Handle(context.Context, Event) (interface{}, error)
}

func LiftHandler(h Handler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		ctx := r.Context()
		ev := Event{
			Body: r.Body,
		}
		result, err := h.Handle(ctx, ev)
		if err != nil {
			status := 500
			if t, ok := err.(*AppError); ok {
				status = t.Status
			}
			w.WriteHeader(status)
			fmt.Fprintf(w, `{"message": %q}`, err.Error())
			return
		}
		if err := json.NewEncoder(w).Encode(result); err != nil {
			fmt.Fprintf(w, `{"message": %q}`, err.Error())
		}
	}
}

func run(addr string) error {
	mux := &http.ServeMux{}
	{
		fmt.Println(shape.Extract(HelloHandler(Hello)))
	}
	mux.Handle("/", LiftHandler(HelloHandler(Hello)))
	return http.ListenAndServe(addr, mux)
}

func main() {
	addr := ":8080"
	if v := os.Getenv("ADDR"); v != "" {
		addr = v
	}
	if err := run(addr); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
