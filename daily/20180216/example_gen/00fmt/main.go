package main

import (
	"fmt"
	"go/types"

	"golang.org/x/tools/go/loader"
)

func main() {
	conf := loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false
		},
	}
	conf.Import("fmt")
	conf.CreateFromFilenames(".", "main.go") // xxx
	prog, _ := conf.Load()

	qf := types.RelativeTo(prog.Created[0].Pkg)
	pkg := prog.Package("fmt").Pkg
	for _, name := range pkg.Scope().Names() {
		fmt.Println(types.ObjectString(pkg.Scope().Lookup(name), qf))
	}
}
