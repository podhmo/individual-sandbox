package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
)

func Hello(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	fmt.Fprintf(w, `{"message": "hello"}`)
}

func main() {
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}

	log.Print("listen ...", port)
	handler := http.HandlerFunc(Hello)
	addr := fmt.Sprintf(":%d", port)
	if err := http.ListenAndServe(addr, handler); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
