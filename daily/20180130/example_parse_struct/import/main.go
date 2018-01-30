package main

import (
	"go/types"
	"github.com/k0kubun/pp"
	"./internal"
)

func main() {
	var imp types.Importer = new(internal.TryImporters)
	pp.Println(imp.Import("fmt"))
}
