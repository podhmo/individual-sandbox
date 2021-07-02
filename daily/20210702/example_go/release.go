// +build release

package main

import (
	"embed"
	"io/fs"
	"log"
	"net/http"
)

//go:generate sh -c "cd frontend; npm run build"
//go:embed frontend/build/*
var content embed.FS

func init() {
	pub, err := fs.Sub(content, "frontend/build")
	if err != nil {
		log.Fatal(err)
	}
	http.Handle("/", http.FileServer(http.FS(pub)))
}
