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
	config.Import("../01pill")
	prog, err := config.Load()
	if err != nil {
		log.Fatal(err)
	}

	for _, p := range prog.AllPackages {
		if p.Pkg.Name() == "pill" {
			if err := deriving.Stringer(p, "Pill", indent.New(os.Stdout)); err != nil {
				log.Fatal(err)
			}
		}
	}
}
