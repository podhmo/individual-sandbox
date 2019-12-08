package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
)

func main() {
	port := os.Args[1]
	http.Handle("/", http.FileServer(http.Dir(".")))
	fmt.Fprintf(os.Stderr, "start: %s\n", port)

	if err := http.ListenAndServe(fmt.Sprintf(":%s", port), nil); err != nil {
		log.Fatal(err)
	}
}
