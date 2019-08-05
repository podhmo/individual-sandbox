package main

import (
	"m/bar"
	"m/boo"
	"m/conf"
	"m/foo"

	"github.com/k0kubun/pp"
)

func main() {
	c := &conf.Config{}
	pp.Println(foo.New(c), bar.New(c), boo.New(c))
}
