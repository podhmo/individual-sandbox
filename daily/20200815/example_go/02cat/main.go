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

	type pair struct {
		reader io.Reader
		close  func() error
	}

	ins := make(chan pair)
	go func() {
		defer close(ins)
		if !isatty.IsTerminal(os.Stdin.Fd()) {
			ins <- pair{reader: os.Stdin}
		}
		for _, filename := range flag.Args() {
			f, err := os.Open(filename)
			if err != nil {
				log.Println("error opening file: err:", err)
			}
			ins <- pair{reader: f, close: f.Close}
		}
	}()

	for p := range ins {
		p := p
		func() {
			if p.close != nil {
				defer func() {
					if err := p.close(); err != nil {
						log.Println("error closing file: err:", err)
					}
				}()
			}
			io.Copy(os.Stdout, p.reader)
		}()
	}
}
