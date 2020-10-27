package main

import (
	"fmt"
	"log"
	"os"

	"github.com/k0kubun/pp"
	"golang.org/x/tools/go/packages"
)

func main() {
	targets := os.Args[1:]
	if err := run(targets); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run(targets []string) error {
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles,
	}
	pkgs, err := packages.Load(cfg, targets...)
	if err != nil {
		return err
	}
	for _, pkg := range pkgs {
		if pkg.Errors != nil {
			fmt.Println("soft error", pkg.Errors)
		}
		fmt.Println(pkg)
		pp.Println(pkg)
	}
	return nil
}
