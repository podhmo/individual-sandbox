package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
)

func main() {
	parseAndRun := func() error {
		port := 44444
		if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
			port = v
		}
		return run(port)
	}
	if err := parseAndRun(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(port int) error {
	addr := fmt.Sprintf(":%d", port)
	log.Printf("listen addr=%s", addr)

	// TODO: graceful shutdown
	return http.ListenAndServe(addr, nil)
}

func setupServer() *http.ServeMux {
	mux := &http.ServeMux{}
	mux.HandleFunc("/", NewWelcomeHandler())
	return mux
}

func NewWelcomeHandler() http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprintf(w, `{"version": "0.0.0", "message": "welcome! this is foo."}`)
	}
}
