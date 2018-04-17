package main

import (
	"fmt"
	"go/build"
	"log"
	"os"
)

func main() {
	pkgname := "golang.org/x/tools/go/loader"
	if err := run(pkgname); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(pkgname string) error {
	ctxt := build.Default
	cwd, err := os.Getwd()
	if err != nil {
		return err
	}
	var mode build.ImportMode
	bp, err := ctxt.Import(pkgname, cwd, mode)
	if err != nil {
		return err
	}

	for _, f := range bp.GoFiles {
		fmt.Println("G", f)
	}
	for _, f := range bp.TestGoFiles { // _test.go inside package
		fmt.Println("T", f)
	}
	for _, f := range bp.XTestGoFiles { // _test.go outside package
		fmt.Println("XT", f)
	}
	return nil
}
