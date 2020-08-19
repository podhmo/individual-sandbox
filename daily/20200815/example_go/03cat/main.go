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

	pr, pw := io.Pipe()
	go func() {
		defer pw.Close()
		if !isatty.IsTerminal(os.Stdin.Fd()) {
			io.Copy(pw, os.Stdin)
		}
		for _, filename := range flag.Args() {
			f, err := os.Open(filename)
			if err != nil {
				log.Println("error opening file: err:", err)
			}
			io.Copy(pw, f)
			f.Close()
		}
	}()

	io.Copy(os.Stdout, pr)
}
