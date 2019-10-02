package main

import (
	"errors"
	"fmt"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	fmt.Fprintln(os.Stderr, "args", args)
	cfg := &packages.Config{
		Mode: packages.NeedFiles | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, args...)
	if err != nil {
		return err
	}
	if packages.PrintErrors(pkgs) > 0 {
		return errors.New("hmm")
	}
	for _, pkg := range pkgs {
		fmt.Println(pkg.ID)
		for _, f := range pkg.GoFiles {
			fmt.Println("	", f)
		}
	}
	return nil
}
