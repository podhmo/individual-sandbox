package main

import (
	"fmt"
	"net/http"
	"os"
)

func handler(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintln(w, `{"message": "hello world"}`)
}

func main() {
	fmt.Fprintln(os.Stderr, "start :3000")
	http.ListenAndServe(":3000", http.HandlerFunc(handler))
}
