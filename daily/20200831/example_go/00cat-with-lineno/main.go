package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	args := os.Args[1:]
	for i, fname := range args {
		if err := func(i int, fname string) error {
			log.Println("open", i, fname)
			f, err := os.Open(fname)
			if err != nil {
				return err
			}
			defer f.Close()
			defer log.Println("close", i, fname)

			s := bufio.NewScanner(f)
			for s.Scan() {
				fmt.Println(i, s.Text())
			}
			if err := s.Err(); err != nil {
				return err
			}
			return nil
		}(i, fname); err != nil {
			return err
		}
	}
	return nil
}
