package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	flag.Parse()
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	cfg := &packages.Config{Mode: packages.LoadSyntax}
	pkgs, err := packages.Load(cfg, flag.Args()...)
	if err != nil {
		fmt.Fprintf(os.Stderr, "load; %v\n", err)
	}
	if packages.PrintErrors(pkgs) > 0 {
		os.Exit(1)
	}

	// Print the names of the source files
	// for each package listed on the command line.
	for _, pkg := range pkgs {
		fmt.Println(pkg.ID, pkg.GoFiles)
	}
	return nil
}
