package main

import (
	"go/importer"
	"log"
)

func main() {
	if err := run("../model"); err != nil {
		log.Fatal(err)
	}
}

func run(src string) error {
	imp := importer.For("source", nil)
	pkg, err := imp.Import(src)
    if err != nil {
        return err
    }

    fset := token.NewFileSet()
	val, err := types.Eval(fset, pkg, token.NoPos, `Person{Name: "foo", Age: 20}`)
	if err != nil {
		return err
    }
}
