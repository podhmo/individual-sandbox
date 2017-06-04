package main

import (
	"io"
	"log"
	"os"
)

func main() {
	filename := os.Args[1]
	f, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	io.Copy(os.Stdout, f)
}
