package main

import (
	"go/build"

	"github.com/k0kubun/pp"
)

func main() {
	bpkg, _ := build.Default.Import("github.com/podhmo/sandbox/example/foo", ".", build.FindOnly)
	pp.ColoringEnabled = false
	pp.Println(bpkg)
}
