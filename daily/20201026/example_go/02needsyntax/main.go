package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

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
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedSyntax,
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
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		enc.Encode(pkg)
		for _, f := range pkg.Syntax {
			// fmt.Println(f.Name.Name, f.Scope)
			for name, ob := range f.Scope.Objects {
				fmt.Println(f.Name.Name, name, fmt.Sprintf("%T", ob.Decl), ob.Kind)
			}
		}
	}
	return nil
}
