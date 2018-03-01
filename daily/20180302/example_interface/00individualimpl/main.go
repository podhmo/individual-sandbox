package main

import (
	"fmt"
	"go/token"

	"go/ast"
)

// in go/ast
/*
// All declaration nodes implement the Decl interface.
type Expr interface {
	Node
	exprNode()
}
*/

type myexpr struct{}

func (x *myexpr) Pos() token.Pos {
	return token.NoPos
}

func (x *myexpr) End() token.Pos {
	return token.NoPos
}
func (x *myexpr) exprNode() {}

func asExpr(expr ast.Expr) {
	fmt.Println(expr.Pos(), expr.End())
}

func main() {
	asExpr(&myexpr{})
}
