package main

import (
	"fmt"
	"go/ast"
	"go/types"

	xloader "golang.org/x/tools/go/loader"
)

func main() {
	conf := xloader.Config{}
	conf.Import(".")
	conf.Import("io")
	info, _ := conf.Load()

	loaderPkg := info.Package("golang.org/x/tools/go/loader").Pkg
	loaderConfig := loaderPkg.Scope().Lookup("Config")

	mainPkg := info.Package(".").Pkg

	{
		fmt.Println("in main package")
		qf := types.RelativeTo(mainPkg)
		fmt.Println("	", types.TypeString(loaderConfig.Type(), qf))
		fmt.Println("	", types.TypeString(types.NewPointer(loaderConfig.Type()), qf))
		fmt.Println("	", types.TypeString(types.NewSlice(loaderConfig.Type()), qf))
		fmt.Println("	", types.TypeString(
			types.NewMap(
				types.Universe.Lookup("string").Type(),
				types.NewPointer(loaderConfig.Type()),
			),
			qf,
		))
	}
	{
		fmt.Println("in loader package")
		qf := types.RelativeTo(loaderPkg)
		fmt.Println("	", types.TypeString(loaderConfig.Type(), qf))
		fmt.Println("	", types.TypeString(types.NewPointer(loaderConfig.Type()), qf))
	}
	fmt.Println("----------------------------------------")
	{
		f := info.Package(".").Files[0]
		for _, is := range f.Imports {
			fmt.Printf("name=%s path=%s\n", is.Name, is.Path.Value)
		}
	}

	fmt.Println("----------------------------------------")
	{
		qf := NameTo(info.Package(".").Pkg, info.Package(".").Files[0])
		fmt.Println("	", types.TypeString(loaderConfig.Type(), qf))
	}

}

// NameTo :
func NameTo(pkg *types.Package, f *ast.File) types.Qualifier {
	return func(other *types.Package) string {
		if pkg == other {
			return "" // same package; unqualified
		}
		// todo: cache
		for _, is := range f.Imports {
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
