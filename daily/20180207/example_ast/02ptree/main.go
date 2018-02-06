package main

import (
	"log"

	"go/ast"

	"../source"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %#v\n", err)
	}
}

func run() error {
	config, err := source.Build()
	if err != nil {
		return err
	}

	// before
	ast.Print(config.Fset, config.Files[0])
	return nil
}
