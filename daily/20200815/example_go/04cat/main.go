package main

import (
	"flag"
	"io"
	"log"
	"os"

	"github.com/mattn/go-isatty"
)

func main() {
	flag.Parse()

	if !isatty.IsTerminal(os.Stdin.Fd()) {
		cat(os.Stdin)
	}
	for _, filename := range flag.Args() {
		f, err := os.Open(filename)
		if err != nil {
			log.Println("error opening file: err:", err)
		}
		cat(f)
	}
}

func cat(r io.Reader) {
	defer func() {
		if r, ok := r.(io.Closer); ok {
			if err := r.Close(); err != nil {
				log.Println("error:", err)
			}
		}
	}()
	io.Copy(os.Stdout, r)
}
