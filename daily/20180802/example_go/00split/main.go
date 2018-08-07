package main

import (
	"log"
	"strings"

	"github.com/k0kubun/pp"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	pp.Println(strings.SplitN("foo:bar/boo", ":", 2))
	pp.Println(strings.Split(strings.SplitN("foo:bar/boo", ":", 2)[1], "/"))
	return nil
}
