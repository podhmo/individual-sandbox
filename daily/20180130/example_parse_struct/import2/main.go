package main

import (
	"encoding/json"
	"go/importer"
	"go/token"
	"go/types"
	"log"
	"os"

	"github.com/k0kubun/pp"
)

func main() {
	// importer.For("gc", nil)
	imp := importer.For("source", nil)
	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	pkg, err := imp.Import("../model")
	if err != nil {
		log.Fatal(err)
	}
	fset := token.NewFileSet()
	val, err := types.Eval(fset, pkg, token.NoPos, `Person{Name: "foo", Age: 20}`)
	if err != nil {
		log.Fatal(err)
	}
	pp.ColoringEnabled = false
	pp.Println(val)
}
