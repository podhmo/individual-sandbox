package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
)

func Upload(dir string) http.HandlerFunc {
	return func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-Type", "application/json")

		f, h, err := req.FormFile("file")
		if err != nil {
			fmt.Fprintf(w, `{"error": "%q"}`, err.Error())
		}
		defer f.Close()

		filename := h.Filename
		wf, err := os.Create(filepath.Join(dir, filename))
		if err != nil {
			fmt.Fprintf(w, `{"error": "%q"}`, err.Error())
		}
		defer wf.Close()
		io.Copy(wf, f) // nolint

		fmt.Fprintf(w, `{"filename": %q}`, filename)
	}
}

func main() {
	mux := http.NewServeMux()
	dir := "."
	mux.HandleFunc("/upload", Upload(dir))
	if err := http.ListenAndServe("127.0.0.1:3333", mux); err != nil {
		log.Fatalf("!! %+v", err)
	}
}
