package main

import (
	"fmt"
	"go/importer"
	"go/token"
	"go/types"
	"log"
)

func main() {
	fset := token.NewFileSet()
	conf := types.Config{
		Importer: importer.Default(),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}
	pkg, err := conf.Check("p", fset, nil, nil)
	if err != nil {
		log.Fatal(err)
	}

	e := &e{fset: fset, pkg: pkg}

	{
		expr := "1 + 1"
		fmt.Println("input:", expr)
		fmt.Println("output:", e.eval(expr).Value)
	}
	{
		expr := "(1 + (2 + (3 + 4) + 5) + 6)"
		fmt.Println("input:", expr)
		fmt.Println("output:", e.eval(expr).Value)
	}
	{
		tv := e.eval("10")
		e.pkg.Scope().Insert(types.NewConst(token.NoPos, e.pkg, "x", tv.Type, tv.Value))

		expr := "x * x"
		fmt.Println("input:", expr)
		fmt.Println("output:", e.eval(expr).Value)
	}
}

type e struct {
	fset *token.FileSet
	pkg  *types.Package
}

func (e *e) eval(s string) *types.TypeAndValue {
	tv, err := types.Eval(e.fset, e.pkg, token.NoPos, s)
	if err != nil {
		log.Fatal(err)
		return nil
	}
	return &tv
}
