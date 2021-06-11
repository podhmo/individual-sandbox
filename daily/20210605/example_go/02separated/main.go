package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"reflect"
	"strconv"
	"time"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	ctx := context.Background()

	bc := NewDefaultBuildContext()
	s := bc.Build()
	return s.Serve(ctx)
}

// handler
func NewHandler() http.Handler {
	var mux http.ServeMux

	mux.Handle("/", hello())         // DI if need
	mux.Handle("/list", helloList()) // DI if need
	return &mux
}

func hello() http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		input := HelloInput{Subject: "world"}
		output, err := Hello(input)
		writeJSON(w, output, err)
	}
}
func helloList() http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		input := HelloInput{Subject: "world"}
		output, err := HelloList(input)
		writeJSONArray(w, output, err)
	}
}

func writeJSON(w http.ResponseWriter, output interface{}, err error) {
	w.Header().Set("Content-Type", "application/json")
	if err != nil {
		w.WriteHeader(500)
		fmt.Fprintf(w, `{"error": %q}`, err.Error()) // TODO: verbose
	}
	if err := json.NewEncoder(w).Encode(output); err != nil {
		w.WriteHeader(500)
		fmt.Fprintf(w, `{"error": %q}`, err.Error())
	}
}
func writeJSONArray(w http.ResponseWriter, output interface{}, err error) {
	type wrapped struct {
		More  bool        `json:"more"`
		Items interface{} `json:"items"`
	}

	w.Header().Set("Content-Type", "application/json")
	if err != nil {
		w.WriteHeader(500)
		fmt.Fprintf(w, `{"error": %q}`, err.Error()) // TODO: verbose
	}

	v := wrapped{Items: output}             // todo: handle `more` attribute
	if reflect.ValueOf(output).Len() == 0 { // slow
		v.Items = make([]int, 0)
	}
	if err := json.NewEncoder(w).Encode(v); err != nil {
		w.WriteHeader(500)
		fmt.Fprintf(w, `{"error": %q}`, err.Error())
	}
}

func Hello(input HelloInput) (*HelloOutput, error) {
	return &HelloOutput{
		Message: fmt.Sprintf("hello %s", input.Subject),
	}, nil
}

type HelloInput struct {
	Subject string
}
type HelloOutput struct {
	Message string `json:"Message"`
}

func HelloList(input HelloInput) ([]HelloOutput, error) {
	return []HelloOutput{
		{Message: fmt.Sprintf("hello %s", input.Subject)},
	}, nil
}

// main
type BuildContext struct {
	ReadHeaderTimeout time.Duration

	Port int
}

func NewDefaultBuildContext() *BuildContext {
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}

	return &BuildContext{
		Port:              port,
		ReadHeaderTimeout: 5 * time.Second,
	}
}

func (bc *BuildContext) Build() *Server {
	h := NewHandler()
	wrapped := http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		// middleware
		log.Printf("accessing %s %s", req.Method, req.URL.Path)
		h.ServeHTTP(w, req)
	})
	return &Server{
		bc:      bc,
		Handler: wrapped,
	}
}

type Server struct {
	bc *BuildContext

	Handler http.Handler
}

func (s *Server) Serve(ctx context.Context) (retErr error) {
	bc := s.bc

	log.Println("listen ...", bc.Port)
	addr := fmt.Sprintf(":%d", bc.Port)
	srv := &http.Server{
		Addr:              addr,
		Handler:           s.Handler,
		ReadHeaderTimeout: s.bc.ReadHeaderTimeout,
	}

	idleConnsClosed := make(chan struct{})
	go func() {
		sigint := make(chan os.Signal, 1)
		signal.Notify(sigint, os.Interrupt)
		<-sigint

		// We received an interrupt signal, shut down.
		if err := srv.Shutdown(ctx); err != nil {
			// Error from closing listeners, or context timeout:
			log.Printf("HTTP server Shutdown: %v", err)
			if retErr != nil {
				retErr = err
			}
		}
		close(idleConnsClosed)
	}()

	if err := srv.ListenAndServe(); err != http.ErrServerClosed {
		// Error starting or closing listener:
		log.Printf("HTTP server ListenAndServe: %v", err)
		return err
	}

	<-idleConnsClosed

	log.Println("exit")
	return nil
}
