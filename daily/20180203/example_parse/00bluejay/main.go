package main

import (
	"fmt"
	"go/build"
	"go/parser"
	"go/types"
	"io/ioutil"
	"log"
	"path/filepath"
	"reflect"
	"strings"

	"golang.org/x/tools/go/loader"
)

// ImportPkg :
func ImportPkg(conf *loader.Config, pkgpath string) []string {
	if strings.HasSuffix(pkgpath, "/*") {
		return importPkgDir(conf, strings.TrimSuffix(pkgpath, "/*"))
	}
	conf.Import(pkgpath)
	return []string{pkgpath}
}

func importPkgDir(conf *loader.Config, rootPkg string) []string {
	var subPkgs []string
	for _, dir := range conf.Build.SrcDirs() {
		fs, err := ioutil.ReadDir(filepath.Join(dir, rootPkg))
		_ = err
		for _, f := range fs {
			if f.IsDir() {
				pkgpath := filepath.Join(rootPkg, f.Name())
				subPkgs = append(subPkgs, pkgpath)
				conf.Import(pkgpath)
			}
		}
	}
	return subPkgs
}

func main() {
	ctxt := build.Default

	conf := &loader.Config{
		Build:       &ctxt,
		ParserMode:  parser.ParseComments,
		AllowErrors: false, // xxx
	}

	rootPkg := "github.com/blue-jay/blueprint/model/*"

	pkgpaths := ImportPkg(conf, rootPkg)
	fmt.Println(pkgpaths)

	prog, err := conf.Load()
	if err != nil {
		log.Fatal(err)
	}
	for _, pkgpath := range pkgpaths {
		info := prog.Package(pkgpath)
		s := info.Pkg.Scope()
		for _, name := range s.Names() {
			ob := s.Lookup(name)

			if internal, ok := ob.Type().Underlying().(*types.Struct); ok {
				fmt.Printf("%s.%s\n", info.Pkg.Name(), name)
				for i := 0; i < internal.NumFields(); i++ {
					dbname, found := reflect.StructTag(internal.Tag(i)).Lookup("db")
					field := internal.Field(i)
					fmt.Printf("	%v (exported=%t, dbname=%s, found=%t)\n", field, field.Exported(), dbname, found)
				}
				fmt.Println("")
			}
		}
	}
}
