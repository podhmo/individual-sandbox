package main

import (
	"fmt"
	"log"
	"os"
)

func run() (ERR error) {
	f, err := os.Create("./hello.txt")
	if err != nil {
		return err
	}
	defer func() {
		if err := f.Close(); err != nil {
			ERR = err
		}
	}()
	fmt.Fprintln(f, "hello")
	return
}

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
