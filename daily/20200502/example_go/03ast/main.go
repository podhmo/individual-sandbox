package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
)

var source = `
package m

type Person struct {Name string}
func NewPerson(name string) *Person {
	return &Person{Name: string}
}

`

func main() {
	if err := run(); err != nil {
		log.Fatal(err)
	}
}

type visiter struct{}

func (v *visiter) Visit(node ast.Node) ast.Visitor {
	fmt.Printf("%#+v\n", node)
	return v
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "xxx.go", source, 0)
	if err != nil {
		return err
	}
	// ast.Print(fset, f)

	// v := &visiter{}
	// ast.Walk(v, f)
	fmt.Printf("lookup: %#+v\n", f.Scope.Lookup("NewPerson"))
	return nil
}

/*
     0  *ast.File {
     1  .  Package: xxx.go:2:1
     2  .  Name: *ast.Ident {
     3  .  .  NamePos: xxx.go:2:9
     4  .  .  Name: "m"
     5  .  }
     6  .  Decls: []ast.Decl (len = 2) {
     7  .  .  0: *ast.GenDecl {
     8  .  .  .  TokPos: xxx.go:4:1
     9  .  .  .  Tok: type
    10  .  .  .  Lparen: -
    11  .  .  .  Specs: []ast.Spec (len = 1) {
    12  .  .  .  .  0: *ast.TypeSpec {
    13  .  .  .  .  .  Name: *ast.Ident {
    14  .  .  .  .  .  .  NamePos: xxx.go:4:6
    15  .  .  .  .  .  .  Name: "Person"
    16  .  .  .  .  .  .  Obj: *ast.Object {
    17  .  .  .  .  .  .  .  Kind: type
    18  .  .  .  .  .  .  .  Name: "Person"
    19  .  .  .  .  .  .  .  Decl: *(obj @ 12)
    20  .  .  .  .  .  .  }
    21  .  .  .  .  .  }
    22  .  .  .  .  .  Assign: -
    23  .  .  .  .  .  Type: *ast.StructType {
    24  .  .  .  .  .  .  Struct: xxx.go:4:13
    25  .  .  .  .  .  .  Fields: *ast.FieldList {
    26  .  .  .  .  .  .  .  Opening: xxx.go:4:20
    27  .  .  .  .  .  .  .  List: []*ast.Field (len = 1) {
    28  .  .  .  .  .  .  .  .  0: *ast.Field {
    29  .  .  .  .  .  .  .  .  .  Names: []*ast.Ident (len = 1) {
    30  .  .  .  .  .  .  .  .  .  .  0: *ast.Ident {
    31  .  .  .  .  .  .  .  .  .  .  .  NamePos: xxx.go:4:21
    32  .  .  .  .  .  .  .  .  .  .  .  Name: "Name"
    33  .  .  .  .  .  .  .  .  .  .  .  Obj: *ast.Object {
    34  .  .  .  .  .  .  .  .  .  .  .  .  Kind: var
    35  .  .  .  .  .  .  .  .  .  .  .  .  Name: "Name"
    36  .  .  .  .  .  .  .  .  .  .  .  .  Decl: *(obj @ 28)
    37  .  .  .  .  .  .  .  .  .  .  .  }
    38  .  .  .  .  .  .  .  .  .  .  }
    39  .  .  .  .  .  .  .  .  .  }
    40  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
    41  .  .  .  .  .  .  .  .  .  .  NamePos: xxx.go:4:26
    42  .  .  .  .  .  .  .  .  .  .  Name: "string"
    43  .  .  .  .  .  .  .  .  .  }
    44  .  .  .  .  .  .  .  .  }
    45  .  .  .  .  .  .  .  }
    46  .  .  .  .  .  .  .  Closing: xxx.go:4:32
    47  .  .  .  .  .  .  }
    48  .  .  .  .  .  .  Incomplete: false
    49  .  .  .  .  .  }
    50  .  .  .  .  }
    51  .  .  .  }
    52  .  .  .  Rparen: -
    53  .  .  }
    54  .  .  1: *ast.FuncDecl {
    55  .  .  .  Name: *ast.Ident {
    56  .  .  .  .  NamePos: xxx.go:5:6
    57  .  .  .  .  Name: "NewPerson"
    58  .  .  .  .  Obj: *ast.Object {
    59  .  .  .  .  .  Kind: func
    60  .  .  .  .  .  Name: "NewPerson"
    61  .  .  .  .  .  Decl: *(obj @ 54)
    62  .  .  .  .  }
    63  .  .  .  }
    64  .  .  .  Type: *ast.FuncType {
    65  .  .  .  .  Func: xxx.go:5:1
    66  .  .  .  .  Params: *ast.FieldList {
    67  .  .  .  .  .  Opening: xxx.go:5:15
    68  .  .  .  .  .  Closing: xxx.go:5:16
    69  .  .  .  .  }
    70  .  .  .  .  Results: *ast.FieldList {
    71  .  .  .  .  .  Opening: -
    72  .  .  .  .  .  List: []*ast.Field (len = 1) {
    73  .  .  .  .  .  .  0: *ast.Field {
    74  .  .  .  .  .  .  .  Type: *ast.StarExpr {
    75  .  .  .  .  .  .  .  .  Star: xxx.go:5:18
    76  .  .  .  .  .  .  .  .  X: *ast.Ident {
    77  .  .  .  .  .  .  .  .  .  NamePos: xxx.go:5:19
    78  .  .  .  .  .  .  .  .  .  Name: "Person"
    79  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 16)
    80  .  .  .  .  .  .  .  .  }
    81  .  .  .  .  .  .  .  }
    82  .  .  .  .  .  .  }
    83  .  .  .  .  .  }
    84  .  .  .  .  .  Closing: -
    85  .  .  .  .  }
    86  .  .  .  }
    87  .  .  .  Body: *ast.BlockStmt {
    88  .  .  .  .  Lbrace: xxx.go:5:26
    89  .  .  .  .  List: []ast.Stmt (len = 1) {
    90  .  .  .  .  .  0: *ast.ReturnStmt {
    91  .  .  .  .  .  .  Return: xxx.go:6:2
    92  .  .  .  .  .  .  Results: []ast.Expr (len = 1) {
    93  .  .  .  .  .  .  .  0: *ast.UnaryExpr {
    94  .  .  .  .  .  .  .  .  OpPos: xxx.go:6:9
    95  .  .  .  .  .  .  .  .  Op: &
    96  .  .  .  .  .  .  .  .  X: *ast.CompositeLit {
    97  .  .  .  .  .  .  .  .  .  Type: *ast.Ident {
    98  .  .  .  .  .  .  .  .  .  .  NamePos: xxx.go:6:10
    99  .  .  .  .  .  .  .  .  .  .  Name: "Person"
   100  .  .  .  .  .  .  .  .  .  .  Obj: *(obj @ 16)
   101  .  .  .  .  .  .  .  .  .  }
   102  .  .  .  .  .  .  .  .  .  Lbrace: xxx.go:6:16
   103  .  .  .  .  .  .  .  .  .  Rbrace: xxx.go:6:17
   104  .  .  .  .  .  .  .  .  .  Incomplete: false
   105  .  .  .  .  .  .  .  .  }
   106  .  .  .  .  .  .  .  }
   107  .  .  .  .  .  .  }
   108  .  .  .  .  .  }
   109  .  .  .  .  }
   110  .  .  .  .  Rbrace: xxx.go:7:1
   111  .  .  .  }
   112  .  .  }
   113  .  }
   114  .  Scope: *ast.Scope {
   115  .  .  Objects: map[string]*ast.Object (len = 2) {
   116  .  .  .  "NewPerson": *(obj @ 58)
   117  .  .  .  "Person": *(obj @ 16)
   118  .  .  }
   119  .  }
   120  .  Unresolved: []*ast.Ident (len = 1) {
   121  .  .  0: *(obj @ 40)
   122  .  }
   123  }
*/
