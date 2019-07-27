package main

import (
	"log"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("+%v", err)
	}
}

type person struct {
	Name    string
	Age     int
	Parents []person
}

func run() error {
	father := person{
		Name: "foo",
		Age:  40,
	}
	mother := person{
		Name: "bar",
		Age:  40,
	}
	me := person{
		Name:    "far",
		Age:     20,
		Parents: []person{father, mother},
	}
	pp.Println(me)
	return nil
}
