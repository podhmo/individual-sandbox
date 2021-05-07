// +build gen

package main

import (
	"fmt"
	"os"
)

func main() {
	f, err := os.Create("foo_list.go")
	if err != nil {
		panic(err)
	}
	defer func() {
		if err := f.Close(); err != nil {
			panic(err)
		}
	}()
	fmt.Fprintln(f, `package foo

var FOO_LIST = []string{"XXX", "YYY", "ZZZ"}
`)
}
