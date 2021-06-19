package main

import (
	"fmt"
	"io"
	"log"
)

type S string

func (v S) Format(f fmt.State, c rune) {
	s := string(v)

	if c == 'v' && f.Flag('#') {
		s = s + " string"
	}
	if c == 'v' && f.Flag('+') {
		s = "[[" + s + "]]"
	}
	io.WriteString(f, s)
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	s := S("foo")
	fmt.Println(s)
	fmt.Printf("%v\n", s)
	fmt.Printf("%+v\n", s)
	fmt.Printf("%#+v\n", s)
	return nil
}
