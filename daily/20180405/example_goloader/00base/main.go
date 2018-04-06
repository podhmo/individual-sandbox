package main

import (
	"go/build"
	"io/ioutil"
	"log"
	"os"
	"time"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func newLoader() *loader.Config {
	ctxt := build.Default
	ctxt.ReadDir = func(path string) ([]os.FileInfo, error) {
		log.Println("	open", path)
		return ioutil.ReadDir(path)
	}

	c := &loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false
		},
		Build: &ctxt,
	}
	c.TypeChecker.DisableUnusedImportCheck = true
	return c
}

func run() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	c := newLoader()
	c.Import("fmt")
	_, err := c.Load()
	return err
}
