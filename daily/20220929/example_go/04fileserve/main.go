package main

import (
	"log"
	"net/http"
	"os"
)

func main() {
	d := os.Args[1]
	root := os.DirFS(d)
	// http.Handle("/downloads/", http.StripPrefix("/downloads/", http.FileServer(http.Dir(d))))
	http.Handle("/downloads/", http.StripPrefix("/downloads/", http.FileServer(http.FS(root))))

	log.Println("listen", 8080)
	if err := http.ListenAndServe("localhost:8080", nil); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
