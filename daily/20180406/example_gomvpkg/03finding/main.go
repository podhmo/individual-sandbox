package main

import (
	"fmt"
	"go/build"
	"go/parser"
	"go/token"
	"log"
	"strconv"
	"strings"
	"time"

	"golang.org/x/tools/go/buildutil"
	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	ctxt := &build.Default
	inpkg := "github.com/podhmo/handwriting"
	srcpkg := "golang.org/x/tools/go/loader"

	// dstpkg := "github.com/xxx/loader"

	// find root
	var rootdir string
	for _, dir := range ctxt.SrcDirs() {
		path := buildutil.JoinPath(ctxt, dir, inpkg)
		if buildutil.IsDir(ctxt, path) {
			rootdir = path
			log.Println("found root", path)
			break
		}
	}

	// find go package in inpkg
	var pkgdirs []string
	q := []string{rootdir}

	for len(q) > 0 {
		dir := q[0]
		q = q[1:]

		fs, err := buildutil.ReadDir(ctxt, dir)
		if err != nil {
			return err
		}

		used := false
		for _, f := range fs {
			if f.IsDir() {
				q = append(q, buildutil.JoinPath(ctxt, dir, f.Name()))
				continue
			}
			if used {
				continue
			}
			if strings.HasSuffix(f.Name(), ".go") {
				log.Println("found ", dir)
				pkgdirs = append(pkgdirs, dir)
				used = true
			}
		}
	}
	type Affected struct {
		dir     string
		name    string
		pkgpath string
		files   []string
	}

	var affected []Affected

	// find affected pkg
	fset := token.NewFileSet()
	for _, dir := range pkgdirs {
		fs, err := buildutil.ReadDir(ctxt, dir)
		if err != nil {
			return err
		}

		pkgpath := dir
		for _, prefix := range ctxt.SrcDirs() {
			if strings.HasPrefix(pkgpath, prefix) {
				pkgpath = pkgpath[len(prefix)+1:]
				break
			}
		}
		current := Affected{dir: dir, pkgpath: pkgpath}
		for _, f := range fs {
			if !strings.HasSuffix(f.Name(), ".go") {
				continue
			}
			func() {
				r, err := buildutil.OpenFile(ctxt, buildutil.JoinPath(ctxt, dir, f.Name()))
				if err != nil {
					log.Println(f.Name(), err)
					return
				}
				defer r.Close()
				astf, err := parser.ParseFile(fset, f.Name(), r, parser.ImportsOnly)
				if err != nil {
					log.Println(f.Name(), err)
					return
				}

				current.name = astf.Name.Name

				for _, is := range astf.Imports {
					path, err := strconv.Unquote(is.Path.Value)
					if err != nil {
						log.Println(f.Name(), err)
					}
					if path == srcpkg {
						current.files = append(current.files, f.Name())
						break
					}
				}
			}()
		}
		if len(current.files) > 0 {
			affected = append(affected, current)
			log.Println("affected", affected)
		}
	}

	// slow
	c := loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			if strings.Contains(path, "/vendor/") {
				return false
			}
			if strings.HasSuffix(path, "_test.go") {
				return false
			}
			return true
		},
	}

	for _, a := range affected {
		c.Import(a.pkgpath)
	}

	st := time.Now()
	prog, err := c.Load()
	fmt.Println(time.Now().Sub(st))
	if err != nil {
		return err
	}
	log.Println(len(prog.AllPackages))
	fmt.Println("ok")

	return nil
}
