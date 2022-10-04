package main

import (
	"fmt"

	"golang.org/x/tools/go/packages"
)

func main() {
	cfg := &packages.Config{
		Mode:  packages.NeedFiles,
		Tests: true,
	}
	pkgpath := "github.com/podhmo/reflect-shape/metadata"
	pkgs, err := packages.Load(cfg, pkgpath)
	if err != nil {
		panic(err)
	}
	if packages.PrintErrors(pkgs) > 0 {
		panic("hmm")
	}
	for _, pkg := range pkgs {
		fmt.Println(pkg)
		fmt.Println("\tfiles", pkg.GoFiles)
		fmt.Println("\tignored", pkg.IgnoredFiles)
		fmt.Println("\tother", pkg.OtherFiles)
	}
}
