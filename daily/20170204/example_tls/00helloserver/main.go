package main

import (
	"fmt"
	"log"
	"net/http"
)

// Hello :
func Hello(w http.ResponseWriter, req *http.Request) {
	w.Header().Set("Content-Type", "text/plain")
	fmt.Fprintf(w, "hello")
}

func main() {
	http.HandleFunc("/", Hello)
	certFile := "../cert.pem"
	keyFile := "../key.pem"
	log.Println("starting -- port 4444")
	log.Fatal(http.ListenAndServeTLS(":4444", certFile, keyFile, nil))
}
