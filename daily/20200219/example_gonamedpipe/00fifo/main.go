package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	// winio "github.com/Microsoft/go-winio"
)

var (
	name = flag.String("name", "foo.pipe", "")
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatal(err)
	}
}
func run() error {
	pipename := *name

	// os.Remove(pipename)
	// err := syscall.Mkfifo(pipename, 0666)
	// if err != nil {
	// 	return err
	// }

	file, err := os.OpenFile(pipename, os.O_CREATE, os.ModeNamedPipe)
	if err != nil {
		return err
	}

	reader := bufio.NewReader(file)

	for {
		line, err := reader.ReadBytes('\n')
		if err != nil {
			if err == io.EOF {
				break
			}
			return err
		}
		fmt.Printf("load string: %s", line)
	}
	return nil
}
