package main

import (
	"log"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(packages.LoadFiles); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run(packages.LoadImports); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run(packages.LoadTypes); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run(packages.LoadSyntax); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run(packages.LoadAllSyntax); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(mode packages.LoadMode) error {
	c := &packages.Config{Mode: mode}
	pkgpath := "golang.org/x/tools/go/types/typeutil"
	// pkgpath := "file=main.go"
	// pkgpath := "name=typeutil"
	pkgs, err := packages.Load(c, pkgpath)

	if err != nil {
		return err
	}
	log.Println(packages.PrintErrors(pkgs))

	log.Println("collected packages", len(pkgs), "mode", mode)
	for _, pkg := range pkgs {
		log.Println(pkg.ID, "I", pkg.Imports, "T", pkg.Types, "S", pkg.Syntax)
		for _, ipkg := range pkg.Imports {
			log.Println("	", ipkg.ID, "T", ipkg.Types)
			break
		}
	}
	return nil
}
