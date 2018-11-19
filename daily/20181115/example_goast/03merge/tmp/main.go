package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"reflect"
	// "go/printer"
	"go/token"
	"log"

	"github.com/podhmo/printer"
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
	"log"
)

func main(){
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
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
	f2, err := parser.ParseFile(fset, "extra", source2, parser.AllErrors)
	if err != nil {
		return err
	}

	ob := f.Scope.Lookup("main")
	if ob.Kind != ast.Fun {
		return fmt.Errorf("unexpected type %s", ob.Kind)
	}
	ob.Decl.(*ast.FuncDecl).Name.Name = "mainInner"

	fn := f2.Scope.Lookup("main").Decl.(*ast.FuncDecl)
	fn.Type.Func = f.Decls[len(f.Decls)-1].End() + 1 // ! side effect
	fmt.Println("@@@", int(fn.Pos()-f.Pos()))
	// fset.File(f.Pos()).AddLine(int(fn.Pos() - f.Pos()))
	fset.File(f.Pos()).AddLine(141)
	fset.File(f.Pos()).AddLine(142)
	f.Decls = append(f.Decls, fn)
	fmt.Println("@@", fset.File(f.Pos()).PositionFor(fn.Pos(), false))

	lines := reflect.ValueOf(fset.File(f.Pos())).Elem().FieldByName("lines")
	for i := 0; i < lines.Len(); i++ {
		fmt.Println(lines.Index(i).Int())
	}

	buf := new(bytes.Buffer)
	// buf := os.Stdout
	if err := printer.Fprint(buf, fset, f); err != nil {
		return err
	}

	// output, err := format.Source(buf.Bytes())
	// if err != nil {
	// 	return err
	// }
	//os.Stdout.Write(output)
	return nil
}
