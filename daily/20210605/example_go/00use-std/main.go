package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
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

	mux.Handle("/", hello()) // DI if need

	return &mux
}

func hello() http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprintf(w, `{"message": "hello"}`)
	}
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
