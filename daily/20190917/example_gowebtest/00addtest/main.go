package main

import (
	"fmt"
	"log"
	"net/http"
	"strconv"
)

// Add
func Add(w http.ResponseWriter, req *http.Request) {
	qs := req.URL.Query()
	var ans int
	for _, vs := range qs {
		for _, v := range vs {
			v, err := strconv.Atoi(v)
			if err != nil {
				w.WriteHeader(http.StatusBadRequest)
				fmt.Fprintf(w, `{"error": %q}`, err.Error())
				return
			}
			ans += v
		}
	}
	fmt.Fprintf(w, `{"ans": %d}`, ans)
}

func main() {
	mux := &http.ServeMux{}
	mux.HandleFunc("/api/add", Add)
	log.Fatal(http.ListenAndServe(":8080", mux))
}
