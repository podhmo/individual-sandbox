package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"io/ioutil"
	"reflect"
	"runtime"
	"strings"
)

type NameSet struct {
	Name    string
	Args    []string
	Returns []string
}

func walkFuncType(typ *ast.FuncType, ns *NameSet) error {
	if typ.Params != nil {
		var names []string
		i := 0
		for _, x := range typ.Params.List {
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
		ns.Args = names
	}
	if typ.Results != nil {
		var names []string
		i := 0
		for _, x := range typ.Results.List {
			if len(x.Names) == 0 {
				names = append(names, fmt.Sprintf("ret%d", i))
				i++
				continue
			}
			for _, ident := range x.Names {
				names = append(names, ident.Name)
			}
		}
		ns.Returns = names
	}
	return nil
}

func InspectFunc(decl *ast.FuncDecl) (NameSet, error) {
	var r NameSet
	r.Name = decl.Name.Name
	if err := walkFuncType(decl.Type, &r); err != nil {
		return r, err
	}
	return r, nil
}

func InspectLit(lit *ast.FuncLit) (NameSet, error) {
	var r NameSet
	r.Name = ""
	if err := walkFuncType(lit.Type, &r); err != nil {
		return r, err
	}
	return r, nil
}

func Hello(
	w io.Writer,
	name string,
) error {
	_, err := fmt.Fprintln(w, "hello", name)
	return err
}

type P struct{}

func (p *P) Hello(w io.Writer, name string) error { return nil }

func ShowInfo(fn interface{}) error {
	rfunc := runtime.FuncForPC(reflect.ValueOf(fn).Pointer())
	name := rfunc.Name()
	filename, lineno := rfunc.FileLine(rfunc.Entry())
	fmt.Println("@@", name, ":", filename, lineno)

	fset := token.NewFileSet()
	mode := parser.ParseComments
	code, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("read file: %w", err)
	}
	f, err := parser.ParseFile(fset, filename, code, mode)
	if err != nil {
		return fmt.Errorf("parsefile: %w", err)
	}

	pos := fset.File(f.Pos()).LineStart(lineno)
	parts := strings.Split(name, ".")
	ob := f.Scope.Lookup(parts[len(parts)-1])
	fmt.Printf("\t pos=%v, lookup object=%v\n", pos, ob != nil)
	if ob != nil {
		if ob.Kind != ast.Fun {
			return fmt.Errorf("lookup error %s is not founction", name)
		}
		ns, err := InspectFunc(ob.Decl.(*ast.FuncDecl))
		if err != nil {
			return fmt.Errorf("inspect func: %w", err)
		}
		fmt.Println("ok", ns)
		return nil
	}
	for _, decl := range f.Decls {
		// fmt.Printf("%T (%d, %d) -> ok=%v\n", decl, decl.Pos(), decl.End(), (decl.Pos() <= pos && pos <= decl.End()))
		if !(decl.Pos() <= pos && pos <= decl.End()) {
			continue
		}

		var retErr error
		decl, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue
		}
		ast.Inspect(decl.Body, func(node ast.Node) bool {
			//fmt.Printf("%T (%d, %d)\n", node, node.Pos(), node.End())
			if target, ok := node.(*ast.FuncLit); ok {
				if !(target.Pos() <= pos && pos <= target.End()) {
					return false
				}
				// 本来はより小さいものを探すべき
				ns, err := InspectLit(target)
				if err != nil {
					retErr = fmt.Errorf("inspect lit: %w", err)
					return false
				}
				fmt.Println("ok", ns)
				return false
			} else {
				return true
			}
		})
		if retErr != nil {
			return retErr
		}

		// method
		if decl.Pos() <= pos && pos <= decl.End() {
			ns, err := InspectFunc(decl)
			if err != nil {
				return fmt.Errorf("inspect func: %w", err)
			}
			fmt.Println("ok", ns)
		}
		return nil
		// ast.Walk(decl, func(node ast.Node) bool {			})
		// ast.Print(fset, decl)
	}
	return fmt.Errorf("not found")
}

func main() {
	if err := run(); err != nil {
		panic(err)
	}
}

// lookup FuncDecl or FuncLit (method?)
func run() error {
	if err := ShowInfo(Hello); err != nil {
		return err
	}

	if err := ShowInfo(func(
		w io.Writer,
		name string,
	) error {
		return nil
	}); err != nil {
		return err
	}

	if err := ShowInfo(new(P).Hello); err != nil {
		return err
	}
	return nil
}
