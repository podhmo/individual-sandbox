package main

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/types"
	"log"

	xloader "golang.org/x/tools/go/loader"
)

func main() {
	conf := xloader.Config{
		Build: &build.Default,
	}

	conf.Import(".")
	prog, err := conf.Load()
	if err != nil {
		log.Fatal(err)
	}

	{
		fmt.Println("in main.go")
		w := W{Prog: prog, File: prog.Package(".").Files[0]}
		loaderQ := w.NameTo(prog.Package(".").Pkg)
		typ := prog.Package("golang.org/x/tools/go/loader").Pkg.Scope().Lookup("Config").Type()
		fmt.Println(types.TypeString(typ, loaderQ))
		fmt.Println(types.TypeString(types.NewPointer(typ), loaderQ))
	}

	fmt.Println("----------------------------------------")

	{
		fmt.Println("in golang.org/x/tools/go/loader/loader.go")
		w := W{Prog: prog, File: prog.Package("golang.org/x/tools/go/loader").Files[0]}
		loaderQ := w.NameTo(prog.Package("golang.org/x/tools/go/loader").Pkg)
		typ := prog.Package("golang.org/x/tools/go/loader").Pkg.Scope().Lookup("Config").Type()
		fmt.Println(types.TypeString(typ, loaderQ))
		fmt.Println(types.TypeString(types.NewPointer(typ), loaderQ))
	}
}

// W :
type W struct {
	Prog *xloader.Program
	File *ast.File
}

// NameTo :
func (w *W) NameTo(pkg *types.Package) types.Qualifier {
	return func(other *types.Package) string {
		if pkg == other {
			return "" // same package; unqualified
		}
		// todo: cache
		for _, is := range w.File.Imports {
			if is.Path.Value[1:len(is.Path.Value)-1] == other.Path() {
				if is.Name != nil {
					return is.Name.String()
				}
				return other.Name()
			}
		}
		return other.Name() // todo: add import
	}
}
