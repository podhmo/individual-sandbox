package main

import (
	"log"
	"os"

	"github.com/podhmo/handwriting/deriving"
	"github.com/podhmo/handwriting/indent"
	"golang.org/x/tools/go/loader"
)

func main() {
	config := &loader.Config{}
	config.Import("../00foo")
	prog, err := config.Load()
	if err != nil {
		log.Fatal(err)
	}

	for _, p := range prog.AllPackages {
		if err := deriving.Stringer(p, "Foo", indent.New(os.Stdout)); err != nil {
			log.Fatal(err)
		}
	}
}
