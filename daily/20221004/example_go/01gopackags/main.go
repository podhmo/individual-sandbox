package main

import (
	"fmt"
	"runtime/debug"
)

type S struct {
	Name string
}

func main() {
	fmt.Println("----------------------------------------")
	F()
}

func F() {
	fmt.Println(debug.ReadBuildInfo())
	// pp.Println(build.Default)
}
