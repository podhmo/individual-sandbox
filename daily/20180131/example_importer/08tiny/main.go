package main

import (
	"fmt"
	"go/token"
	"go/types"
	"log"
)

func main() {
	fset := token.NewFileSet()
	expr := "1 * 2 + 3 / 4.0"
	tv, err := types.Eval(fset, nil, token.NoPos, expr)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(expr, "=", tv.Value)
}
