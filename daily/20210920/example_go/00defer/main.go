package main

import (
	"fmt"
	"log"
)

func main() {
	log.Println(run())
}

func run() (err error) {
	foo := &Foo{}
	defer foo.CommitWith(&err)
	return nil
}

type Foo struct{}

func (f *Foo) CommitWith(errPtr *error) {
	err := fmt.Errorf("failed")
	*errPtr = err
}
