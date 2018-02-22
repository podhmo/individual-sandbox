package main

import (
	"github.com/k0kubun/pp"
	"github.com/koding/multiconfig"
)

func main() {
	m := multiconfig.NewWithPath("../config.toml")
	pp.ColoringEnabled = false
	pp.Println(m)
}
