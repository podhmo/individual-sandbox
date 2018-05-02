package main

import (
	"io"
	"log"
	"os"

	_ "./statik" // まじめにやるならabsolute path
	"github.com/rakyll/statik/fs"
)

func main() {
	FS, err := fs.New()
	if err != nil {
		log.Fatal(err)
	}
	f, err := FS.Open("/message.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	if _, err := io.Copy(os.Stdout, f); err != nil {
		log.Fatal(err)
	}
}
