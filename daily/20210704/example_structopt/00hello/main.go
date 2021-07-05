package main

import (
	"flag"
	"fmt"
	"log"
)

type Options struct {
	Name string
}

func main() {
	var opt Options
	flag.StringVar(&opt.Name, "name", "", "greeting target")
	flag.Parse()

	if err := run(&opt); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(opt *Options) error {
	Hello(opt.Name)
	return nil
}

func Hello(name string) {
	fmt.Printf("Hello %s", name)
}
