package main

import (
	"go/build"

	"github.com/k0kubun/pp"
	"golang.org/x/tools/refactor/importgraph"
)

func main() {
	f, r, errors := importgraph.Build(&build.Default)
    pp.ColoringEnabled = false
	pp.Println(f, r, errors)
}
