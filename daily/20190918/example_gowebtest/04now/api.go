package main

import (
	"fmt"
	"net/http"
	"time"
)

func NowHandler(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, `{"now": "%s"}`, time.Now().Format(time.RFC3339))
}

func Handler() http.Handler {
	var mux http.ServeMux
	mux.HandleFunc("/now", NowHandler)
	return &mux
}

func main() {
	h := Handler()
	http.ListenAndServe(":8080", h)
}
