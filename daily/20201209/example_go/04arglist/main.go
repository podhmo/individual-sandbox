package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"log"
)

type Result struct {
	Name    string
	Args    []string
	Returns []string
}

func InspectFunc(f *ast.File, name string) (Result, error) {
	var r Result
	ob := f.Scope.Lookup(name)
	if ob == nil {
		return r, fmt.Errorf("not found %q", name)
	}
	decl, ok := ob.Decl.(*ast.FuncDecl)
	if !ok {
		return r, fmt.Errorf("unexpected decl %T", ob)
	}

	r.Name = decl.Name.Name
	if decl.Type.Params != nil {
		var names []string
		i := 0
		for _, x := range decl.Type.Params.List {
			if len(x.Names) == 0 {
				names = append(names, fmt.Sprintf("arg%d", i))
				i++
				continue
			}
			if _, ok := x.Type.(*ast.Ellipsis); ok {
				names = append(names, fmt.Sprintf("*%s", x.Names[0].Name))
				continue
			}
			for _, ident := range x.Names {
				names = append(names, ident.Name)
			}
		}
		r.Args = names
	}
	if decl.Type.Results != nil {
		var names []string
		i := 0
		for _, x := range decl.Type.Results.List {
			if len(x.Names) == 0 {
				names = append(names, fmt.Sprintf("ret%d", i))
				i++
				continue
			}
			for _, ident := range x.Names {
				names = append(names, ident.Name)
			}
		}
		r.Returns = names
	}
	return r, nil
}

const code = `package foo
func Sum(x int, y,z int) int {
	return x + y + z
}
func Sum2(xs ...int) int {
	return 0
}
func Sprintf(ctx context.Context, fmt string, vs ...interface{}) (string, error) {
	return fmt.Sprintf(fmt, vs...), nil
}
func Sprintf2(ctx context.Context, fmt string, vs ...interface{}) (s string, err error) {
	return fmt.Sprintf(fmt, vs...), nil
}
`

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "foo.go", code, parser.ParseComments)
	if err != nil {
		return err
	}
	fmt.Println(InspectFunc(f, "Sum"))
	fmt.Println(InspectFunc(f, "Sum2"))
	fmt.Println(InspectFunc(f, "Sprintf"))
	fmt.Println(InspectFunc(f, "Sprintf2"))
	return nil
}
