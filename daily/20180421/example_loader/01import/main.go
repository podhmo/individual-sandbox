package main

import (
	"fmt"
	"go/types"
	"log"
	"strings"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	c := loader.Config{}
	c.Import("time")
	prog, err := c.Load()
	if err != nil {
		return err
	}
	for _, pkg := range prog.InitialPackages() {
		s := pkg.Pkg.Scope()
		for _, name := range s.Names() {
			ob := s.Lookup(name)
			if t, _ := ob.Type().Underlying().(*types.Struct); t != nil {
				fmt.Println(
					strings.Replace(
						strings.Replace(
							strings.Replace(ob.String(),
								";", ";\n", -1),
							"{", " {\n ", -1),
						"}", "\n}\n", -1),
				)
			}
		}
		fmt.Println("----------------------------------------")
		for _, im := range pkg.Pkg.Imports() {
			fmt.Println(im)
		}
		fmt.Println("-- all ---------------------------------")
		for _, info := range prog.AllPackages {
			fmt.Println(info.Pkg)
		}
	}
	return nil
}
