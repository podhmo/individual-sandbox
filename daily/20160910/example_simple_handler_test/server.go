package main

import (
	"fmt"
	"net/http"
)

func hello(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "hello world")
}

func main(){
    // using: DefaultServeMux
    http.HandleFunc("/", hello)

    // using: default
    http.ListenAndServe(":8080", nil)
}
