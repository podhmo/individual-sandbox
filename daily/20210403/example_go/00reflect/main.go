package main

import (
	"fmt"
	"log"
	"reflect"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Person struct {
	Name string
}

func run() error {
	ob := Person{}
	rt := reflect.TypeOf(ob)
	rv := reflect.New(rt).Elem()
	fmt.Printf("%#+v\n", rv)
	return nil
}
