package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"log"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	fmt.Fprintln(os.Stderr, "args", args)
	fset := token.NewFileSet()
	cfg := &packages.Config{
		Fset: fset,
		// Mode: packages.LoadAllSyntax,
		Mode: packages.NeedFiles | packages.NeedSyntax | packages.NeedTypes,
	}
	pkgs, err := packages.Load(cfg, args...)
	if err != nil {
		return err
	}

	packages.PrintErrors(pkgs)

	for _, pkg := range pkgs {
		fmt.Fprintln(os.Stderr, pkg.ID)
		for _, f := range pkg.Syntax {
			fmt.Fprintln(os.Stderr, f.Name, fileName(fset, f))
			ch := AllMethods(f)

			for method := range ch {
				fmt.Printf("\t%s\t%s\n", typeName(method.Recv.List[0].Type), method.Name.Name)
			}
			// dorain
			for range ch {
			}
		}
	}
	return nil
}

func fileName(fset *token.FileSet, t *ast.File) string {
	f := fset.File(t.Pos())
	if f == nil {
		return "-"
	}
	return f.Name()
}

func typeName(t ast.Expr) string {
	if t == nil {
		return "<nil>"
	}

	switch t := t.(type) {
	// case *ast.BadExpr:
	case *ast.Ident:
		return t.Name
	// case *ast.Ellipsis:
	// case *ast.BasicLit:
	// case *ast.FuncLit:
	// case *ast.CompositeLit:
	// case *ast.ParenExpr:
	// case *ast.SelectorExpr:
	// case *ast.IndexExpr:
	// case *ast.SliceExpr:
	// case *ast.TypeAssertExpr:
	// case *ast.CallExpr:
	case *ast.StarExpr:
		return "*" + typeName(t.X)
	// case *ast.UnaryExpr:
	// case *ast.BinaryExpr:
	// case *ast.KeyValueExpr:
	// case *ast.ArrayType:
	// case *ast.StructType:
	// case *ast.FuncType:
	// case *ast.InterfaceType:
	// case *ast.MapType:
	// case *ast.ChanType:
	default:
		panic(fmt.Sprintf("unexpected type %T", t))
	}
}

// todo: signature

func AllMethods(f *ast.File) <-chan *ast.FuncDecl {
	ch := make(chan *ast.FuncDecl, 1)
	go func() {
		defer close(ch)
		for _, decl := range f.Decls {
			decl, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			if decl.Recv == nil {
				continue
			}
			// decl.Recv.List[0].Type
			ch <- decl
		}
	}()
	return ch
}
