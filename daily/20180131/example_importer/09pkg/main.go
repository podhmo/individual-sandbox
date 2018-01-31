package main

import (
	"fmt"
	"go/constant"
	"go/token"
	"go/types"
	"log"
)

func main() {
	fset := token.NewFileSet()
	pkg := types.NewPackage("<dummy>", "p")
	inttype := types.Universe.Lookup("int64").Type()
	pkg.Scope().Insert(types.NewConst(token.NoPos, pkg, "x", inttype, constant.MakeInt64(10)))

	expr := "x * x"

	tv, err := types.Eval(fset, pkg, token.NoPos, expr)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(expr, "=", tv.Value)
}
