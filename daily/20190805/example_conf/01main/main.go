package main

import (
	"m/01main/internal"
	"m/conf"

	"github.com/k0kubun/pp"
)

func main() {
	c := &conf.Config{}
	root := internal.InitializeRoot(c)
	pp.Println(root)
}
