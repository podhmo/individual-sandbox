package main

import (
	"fmt"
	"go/build"

	"golang.org/x/tools/go/buildutil"
)

func main() {
	buildutil.ForEachPackage(&build.Default, func(path string, err error) {
		fmt.Println(path)
	})
}
