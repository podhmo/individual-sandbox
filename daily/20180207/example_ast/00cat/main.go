package main

import (
	"fmt"
	"log"
	"os"

	"go/printer"

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
	printer.Fprint(os.Stdout, config.Fset, config.Files[0])

	fmt.Println("----------------------------------------")

	// after
	printer.Fprint(os.Stdout, config.Fset, config.Files[1])
    return nil
}
