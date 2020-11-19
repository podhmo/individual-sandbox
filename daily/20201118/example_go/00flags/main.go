package main

import (
	"flag"
	"fmt"
	"log"
)

func main() {
	var option struct {
		name string
		age  int
	}
	flag.StringVar(&option.name, "name", "", "-")
	flag.IntVar(&option.age, "age", 0, "-")

	run := func() error {
		flag.Parse()

		return Run(option.name, option.age)
	}
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Run(name string, age int) error {
	fmt.Printf("%s(%d): hello\n", name, age)
	return nil
}
