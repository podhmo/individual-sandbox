package main

import (
	"flag"
	"fmt"
	"log"
	"os"
)

type Option struct {
	Name string
}

func main() {
	opt := &Option{}
	cmd := flag.NewFlagSet("app", flag.ContinueOnError)

	cmd.StringVar(&opt.Name, "name", "foo", "name of person")
	cmd.StringVar(&opt.Name, "n", "foo", "(shorthand of --name)")

	if err := cmd.Parse(os.Args[1:]); err != nil {
		if err != flag.ErrHelp {
			cmd.Usage()
		}
		os.Exit(1)
	}
	if err := run(opt); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(opt *Option) error {
	Hello(opt.Name)
	return nil
}

func Hello(name string) {
	fmt.Printf("Hello %s\n", name)
}
