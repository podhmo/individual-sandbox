package main

import (
	"fmt"
	"net/http"
	"time"
)

func Now(now func() time.Time) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		fmt.Fprintf(w, `{"now": "%s"}`, now().Format(time.RFC3339))
	}
}

func Handler(
	now func() time.Time,
) http.Handler {
	var mux http.ServeMux
	mux.HandleFunc("/now", Now(now))
	return &mux
}

func main() {
	h := Handler(time.Now)
	http.ListenAndServe(":8080", h)
}
