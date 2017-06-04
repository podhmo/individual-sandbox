package main

import (
	"io"
	"log"
	"os"
)

func main() {
	filename := "$HOME/.screenrc"
	f, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	io.Copy(os.Stdout, f)
}
