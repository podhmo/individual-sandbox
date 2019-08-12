package main

import (
	"github.com/google/shlex"
	"github.com/k0kubun/pp"
)

func main() {
	pp.Println(shlex.Split(`body = "this is a pen"`))
	pp.Println(shlex.Split(`body <> "this is a pen"`))
}
