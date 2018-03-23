package main

import (
	"fmt"
	"go/types"
	"log"
	"sort"

	"golang.org/x/tools/go/loader"
)

func main() {
	c := loader.Config{
		TypeCheckFuncBodies: func(path string) bool { return false },
	}
	c.Import("bytes")

	prog, err := c.Load()
	if err != nil {
		log.Fatal(err)
	}

	info := prog.Package("bytes")
	ob := info.Pkg.Scope().Lookup("Buffer")

	if named, _ := ob.Type().(*types.Named); named != nil {
		var methods []*types.Func
		for i := 0; i < named.NumMethods(); i++ {
			methods = append(methods, named.Method(i))
		}

		sort.Slice(methods, func(i, j int) bool { return methods[i].Name() < methods[j].Name() })

		for _, m := range methods {
			fmt.Println(m)
		}
	}
}
