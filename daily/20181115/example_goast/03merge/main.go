package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"log"
	"os"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	source := `
package main

import (
	"fmt"
	"log"
)

func main(){
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	fmt.Println("hello")
	return nil
}
`
	source2 := `
package p
func main() {
	fmt.Println("start")
	defer fmt.Println("end")
	mainInner()
}
`
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "main", source, parser.AllErrors)
	if err != nil {
		return err
	}
	f2, err := parser.ParseFile(fset, "p", source2, parser.AllErrors)
	if err != nil {
		return err
	}

	ob := f.Scope.Lookup("main")
	if ob.Kind != ast.Fun {
		return fmt.Errorf("unexpected type %s", ob.Kind)
	}
	ob.Decl.(*ast.FuncDecl).Name.Name = "mainInner"

	fn := f2.Scope.Lookup("main").Decl.(*ast.FuncDecl)
	f.Decls = append(f.Decls, fn)

	buf := new(bytes.Buffer)
	// buf := os.Stdout
	if err := printer.Fprint(buf, fset, f); err != nil {
		return err
	}

	output, err := format.Source(buf.Bytes())
	if err != nil {
		return err
	}
	os.Stdout.Write(output)
	return nil
}
