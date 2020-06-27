package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"os"
	"path"
	"sort"

	"github.com/k0kubun/pp"
	"github.com/pkg/errors"
	"golang.org/x/tools/go/packages"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(args []string) error {
	fset := token.NewFileSet()
	pkg, err := LoadPackage(fset, args[0])
	if err != nil {
		return err
	}
	fmt.Println(pkg)
	p := pkg.Types // *types.Package

	fmt.Println("----------------------------------------")
	sort.Slice(pkg.Syntax, func(i, j int) bool {
		return pkg.Syntax[i].Pos() < pkg.Syntax[j].Pos()
	})
	for _, astf := range pkg.Syntax {
		fmt.Println(fileName(pkg.Fset, astf), astf.Pos(), astf.End())
		if fileName(pkg.Fset, astf) == "models.go" {
			pp.Println(astf)
		}
	}

	fmt.Println("----------------------------------------")
	for _, name := range p.Scope().Names() {
		ob := p.Scope().Lookup(name)

		// find base
		base := ob.Type()
		for {
			underlying := base.Underlying()
			if underlying == base {
				break
			}
			base = underlying
		}

		if t, ok := base.(*types.Struct); ok {
			f := bsearch(pkg.Syntax, ob.Pos())
			fmt.Println(name, t, ob.Pos(), fileName(pkg.Fset, f))

			astOb := f.Scope.Lookup(name)
			if astOb.Kind == ast.Typ {

				doc := astOb.Decl.(*ast.TypeSpec).Doc
				if doc == nil {
					decl := bsearch2(f.Decls, astOb.Pos())
					doc = decl.(*ast.GenDecl).Doc
				}
				fmt.Println("\t@", doc)
			}
			// Bad, Pkg, Con, Fun, Lbl
		}
	}

	fmt.Println("----------------------------------------")
	fmt.Println("xxx", bsearch(pkg.Syntax, token.Pos(10000)))
	fmt.Println("xxx2", bsearch(pkg.Syntax, token.Pos(0)))

	return nil
}

func bsearch(files []*ast.File, pos token.Pos) *ast.File {
	n := len(files)
	low, high := 0, n
	for low < high {
		mid := int(uint(low+high) >> 1) // avoid overflow when computing mid
		if pos < files[mid].Pos() {
			high = mid - 1
		} else if files[mid].End() < pos {
			low = mid + 1
		} else {
			low = mid
			high = mid
		}
	}

	if low == n {
		return nil
	}
	if files[low].Pos() <= pos && pos <= files[low].End() {
		return files[low]
	}
	return nil
}

func bsearch2(decls []ast.Decl, pos token.Pos) ast.Decl {
	n := len(decls)
	low, high := 0, n
	for low < high {
		mid := int(uint(low+high) >> 1) // avoid overflow when computing mid
		if pos < decls[mid].Pos() {
			high = mid - 1
		} else if decls[mid].End() < pos {
			low = mid + 1
		} else {
			low = mid
			high = mid
		}
	}

	if low == n {
		return nil
	}
	if decls[low].Pos() <= pos && pos <= decls[low].End() {
		return decls[low]
	}
	return nil
}

func fileName(fset *token.FileSet, t *ast.File) string {
	if t == nil {
		return "-"
	}

	f := fset.File(t.Pos())
	if f == nil {
		return "-"
	}
	return path.Base(f.Name())
}

// LoadPackage :
func LoadPackage(fset *token.FileSet, importPath string) (*packages.Package, error) {
	cfg := &packages.Config{
		Fset: fset,
		Mode: packages.NeedName | packages.NeedTypes | packages.NeedSyntax,
	}
	pkgs, err := packages.Load(cfg, importPath)
	if err != nil {
		return nil, err
	}
	if len(pkgs) != 1 {
		return nil, errors.Errorf("something wrong: must be len(pkgs) == 1, but %d", len(pkgs))
	}
	return pkgs[0], nil
}
